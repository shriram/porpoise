#lang web-server

(require web-server/servlet-env
         web-server/templates)
(require json)
(require xml)

(require "config.rkt")
(require "synthesize.rkt")
(require "runner.rkt")
(require "problems.rkt")
(require "logger.rkt")

(provide interface-version)
(define interface-version 'stateless)

(define warning-comment "
<!--

⚠    ACHTUNG! ALLES LOOKENSPEEPERS!    ⛔

👁    Looking for something?    👀

-->")

(initialize-problem-list)

(define HOW-MANY-SOLUTIONS 3)

;; sexp->jsexpr :: sexp -> jsexpr
#|
Symbols other than 'null (jsnull) are not handled by write-json,
so this hardens the sexp by converting symbols into
appropriate-looking strings.
It does NOT distinguish 'null, because the intent here is that
the input is an sexp, not a jsexp.
|#

#|
NOTE: There's no good way to represent missing values,
so if recognizes the `json-null` exported by JSON and
turns that into a null. Of course, this is not entirely safe.
|#

(define (sexp->jsexpr sexp)
  (cond
    [(equal? sexp (json-null)) sexp]  ;; for missing values
    [(number? sexp) sexp]
    [(string? sexp) sexp]
    [(boolean? sexp) sexp]
    [(symbol? sexp)
     (string-append "'" (symbol->string sexp))]
    [(list? sexp)
     (cond
       [(empty? sexp) sexp]
       [(eq? (first sexp) 'quote) (map sexp->jsexpr (second sexp))]
       [else (map sexp->jsexpr sexp)])]
    [(hash? sexp)
     (hash-map/copy sexp (lambda (k v) (values k (sexp->jsexpr v))))]
    [else (error 'raw-test->jsexpr "does not yet handle ~s" sexp)]))

(define (raw-test->jsexpr ts)
  (define (sexp->string s)
    (with-output-to-string (λ () (pretty-write s))))
  (make-hasheq `((inputs . ,(map sexp->string (rest (rest ts))))
                 (output . ,(sexp->string (second ts))))))

(define ((respond-to-example-request uid) req)
  (let* ([problem (extract-binding/single 'problem (request-bindings req))]
         [the-prob (get-problem problem global-problem-list)])
    (let* ([bad-impl (problem-bad-impl the-prob)]
           [public-tests (problem-raw-public-test-suite the-prob)]
           [test-suite-as-jsexpr (map raw-test->jsexpr public-tests)])
      (let ([show-bad-impl
             ;; Working assumptions:
             ;; - uids are strings ending in a digit
             ;; - problem IDs are strings ending in a digit
             ;; - uids' last digits are roughly balanced between odd and even
             ;; - problem IDs are roughly balanced between odd and even
             ;; [reason for using odd/even rather than 0-4 vs 5-9 is because there may be many fewer than 10 problems]
             (let ([half-the-end-chars '("0" "2" "4" "6" "8")]
                   [last-char-student-id (substring uid     (sub1 (string-length uid))     (string-length uid))]
                   [last-char-problem-id (substring problem (sub1 (string-length problem)) (string-length problem))])
               ;; Policy:
               ;; If uid has even digit: SHOW bad impl if problem has even digit, else do NOT show.
               ;; If uid has odd  digit: do NOT show bad impl if problem has even digit, else SHOW.
               (if (member last-char-student-id half-the-end-chars)
                   (member last-char-problem-id half-the-end-chars)
                   (not (member last-char-problem-id half-the-end-chars))))])
        (if show-bad-impl
            (begin
              (LOG #:user uid #:problem problem #:context 'returning-examples-with-bad-impl)
              (response/output
               (λ (op) (write-json (make-hasheq `((type . "bad-impl-and-egs")
                                                  (bad-impl . ,bad-impl)
                                                  (examples . ,test-suite-as-jsexpr)))
                                   op))))
            (begin
              (LOG #:user uid #:problem problem #:context 'returning-examples)
              (response/output
               (λ (op) (write-json (make-hasheq `((type . "egs-only")
                                                  (examples . ,test-suite-as-jsexpr)))
                                   op)))))))))

(define ((respond-to-prompt-evaluation-request uid) req)
  (let* ([problem (extract-binding/single 'problem (request-bindings req))]
         [prompt  (extract-binding/single 'prompt  (request-bindings req))]
         [the-prob (get-problem problem global-problem-list)])
    (LOG #:user uid #:problem problem #:context 'prompt prompt)
    (define synthesized-solutions
      (map (λ (_)
             (synthesize (problem-synthesized-name the-prob) prompt))
           (range HOW-MANY-SOLUTIONS)))
    (define public-tests  (problem-public-test-suite  the-prob))
    (define private-tests (problem-private-test-suite the-prob))
    (LOG #:user uid #:problem problem #:context 'synthesized-solutions
         (map (λ (sol)
                (cond [(openai/non-code? sol)       'non-code]
                      [(openai/server-error? sol)   'server-error]
                      [(openai/server-timeout? sol) 'server-timeout]
                      [(openai/code? sol)           (openai/code-code-sexp sol)]
                      [else "ERROR:something-unexpected"]))
              synthesized-solutions))
    (response/output
     (λ (op)
       (let ([test-results (gen-result-matrix synthesized-solutions public-tests private-tests)])
         ;; we can't tell how many tests there were syntactically, since users can write a full test suite
         ;; but it's useful for the UI to have a count of how many tests
         ;; we assume the # of tests is constant across all runs
         ;; not every run corresponds to a valid program and hence produces a valid run
         ;; so we look for the first valid run and extract its counts
         (LOG #:user uid #:problem problem #:context 'result-matrix test-results)
         (let ([valid-outs (filter (lambda (run)
                                     (string=? (hash-ref run 'type) "code"))
                                   test-results)])
           (if (empty? valid-outs)
               (write-json (make-hasheq `((type . "no-runs") (info . ,(sexp->jsexpr test-results)))) op)
               (let* ([first-out (first valid-outs)]
                      [info (hash-ref first-out 'info)]
                      [priv-count (hash-ref (hash-ref info 'private) 'count)]
                      [pub-count (hash-ref (hash-ref info 'public) 'count)]
                      [output (make-hasheq `((type . "runs")
                                             (priv-count . ,(number->string priv-count))
                                             (pub-count . ,(number->string pub-count))
                                             (info . ,test-results)))])
                 (write-json (sexp->jsexpr output) op)))))))))

(define name-for-login-token "Porpoise Login ID")

(define (start req)
  (define init-id
    (with-handlers ([exn:fail? (lambda (_) "")])
      (extract-binding/single 'init-id (request-bindings req))))
  (let loop ([first-time? #t])
    (let ([login-text-to-display
           (if first-time?
               ""
               (string-append "Your " name-for-login-token " was not recognized. Please try again."))])
      (let ([r (send/suspend
                (lambda (k-url)
                  (response/output
                   (lambda (op)
                     (display (include-template "login-template.html") op)))))])
        (let ([uid (with-handlers ([exn:fail? (lambda (_)
                                                (error 'start "binding uid not present"))])
                     (extract-binding/single 'uid (request-bindings r)))])
          (if (member uid (with-input-from-file (path-maker "logins.sexp")
                            (lambda ()
                              (let loop ()
                                (let ([r (read-line)])
                                  (if (eof-object? r)
                                      '()
                                      (cons r (loop))))))))
              (begin
                (LOG #:user uid #:problem NO-PROBLEM #:context 'successful-login uid)
                (send/suspend/dispatch
                 (lambda (embed-url)
                   (let ([problem-names (map problem-public-name global-problem-list)]
                         [examples-response-point
                          (embed-url (respond-to-example-request uid))]
                         [evaluation-response-point
                          (embed-url (respond-to-prompt-evaluation-request uid))])
                     (response/output
                      (λ (op) (display (include-template "porpoise-template.html") op)))))))
              (begin
                (LOG #:user NO-USER #:problem NO-PROBLEM #:context 'failed-login uid)
                (loop #f))))))))

(when (check-for-api-key) (displayln "API key found."))
(serve/servlet start
               #:listen-ip #f
               #:stateless? #t
               #:extra-files-paths (list (path-maker))
               )
