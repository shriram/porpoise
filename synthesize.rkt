#lang web-server

(require racket/serialize)

(provide synthesize check-for-api-key)
(provide [struct-out openai/code]
         [struct-out openai/non-code]
         [struct-out openai/server-timeout]
         [struct-out openai/server-error])

(require json)
(require net/http-client)
(require racket/engine)

;; Places that dispatch on the different response types:
;; - server.rkt

(serializable-struct openai/response       ())
(serializable-struct openai/code           openai/response (code-sexp #|SExp starting with "(define"|# ) #:transparent)
(serializable-struct openai/non-code       openai/response (output #|core response|# )                   #:transparent)
(serializable-struct openai/server-timeout openai/response ()                                            #:transparent)
(serializable-struct openai/server-error   openai/response (diagnostic #|full response sexp|# )          #:transparent)

(define api-key (getenv "OPENAI_API_KEY"))

(define (check-for-api-key)
  (if api-key
      #t
      (error "You must set the environment variable OPENAI_API_KEY to your OpenAI API key.")))

(define timeout-time
  (let ([env (getenv "PORPOISE_TIMEOUT_MS")])
    (if env
        (let ([n (string->number env)])
          (if (and n (integer? n) (positive? n))
              n
              (error 'timeout-time "Value `~a` for PORPOISE_TIMEOUT_MS is not valid: needs to be a positive integer." env)))
        7000))) ;; default timeout; worked well for Brown

(define system-prompt
  "You are a programming assistant that generates programs in the Racket programming language.
   Your response should contain *only* a Racket program. It should NOT include anything else:
   explanation, test cases, or anything else.
   The output should be a Racket function that can be evaluated directly.
   It should begin with \"(define\" and end with \")\", e.g.,
   (define (f x) x),
   but replaced with the actual function you produce.")

;; Symbol * String -> SExp U false
(define (synthesize function-name user-prompt)
  (define host "api.openai.com")
  (define uri "/v1/chat/completions")
  (define headers
    (list "Content-Type: application/json"
          (string-append "Authorization: Bearer " api-key)))

  (define request-body (make-hasheq))
  (hash-set! request-body 'model "gpt-3.5-turbo")
  (hash-set! request-body 'temperature 0.8)
  (hash-set! request-body
             'messages
             (list
              (hasheq 'role "system" 'content system-prompt)
              (hasheq 'role "user"   'content (string-append "Define a function named `"
                                                             (symbol->string function-name)
                                                             "` that meets the following specification:"))
              (hasheq 'role "user"   'content user-prompt)
              ))

  (define e
    (engine
     (lambda (_)
       (define-values (response-status response-headers ip)
         (http-sendrecv host
                        uri
                        #:ssl? #t
                        #:method "POST"
                        #:headers headers
                        #:data (jsexpr->string request-body)))

       (define parsed-response-jx (read-json ip))
       (pretty-print parsed-response-jx)
       (flush-output)

       (cond
         [(hash-ref parsed-response-jx 'choices #f)
          (let ([core-content (hash-ref (hash-ref (car (hash-ref parsed-response-jx 'choices)) 'message) 'content)]
                [r (regexp-quote "(define")])
            (with-handlers ([exn:fail? (lambda (_) (openai/non-code (string-append "read error in: " core-content)))])
              (if (regexp-match r (read-line (open-input-string core-content)))
                  (let ([possible-code (read (open-input-string core-content))])
                    (let ([ns (make-base-namespace)])
                      (eval '(require racket) ns)  ;; make sure this matches what is in `run-test-suite` in «runner.rkt»
                      (with-handlers ([exn:fail? (lambda (_) (openai/non-code (string-append "eval error in: " core-content)))])
                        (eval possible-code ns)
                        (openai/code possible-code))))
                  (openai/non-code core-content))))]
         [else
          (openai/server-error parsed-response-jx)]))))

  (define o (engine-run timeout-time e))
  (if o
      (engine-result e)
      (openai/server-timeout)))
