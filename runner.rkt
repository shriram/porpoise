#lang web-server

(provide gen-result-matrix)

(require rackunit)
(require racket/engine)
(require racket/sandbox)
(require "synthesize.rkt")

;; gen-result-matrix :: (listof openai/response) * test-suite[public] * test-suite[private] -> result-matrix
(define (gen-result-matrix loresps pub-ts priv-ts)

  (define (gen-one-result resp)
    (cond
      [(openai/code? resp)
       (let ([code (openai/code-code-sexp resp)])
         (let ([out-public  (run-test-suite pub-ts code #:include-failure-details? #t)]
               [out-private (run-test-suite priv-ts code #:include-failure-details? #f)])
           (if (or (eq? out-public 'fail) (eq? out-private 'fail))
               (make-hasheq `((type . "eval-error")))
               (make-hasheq `((type . "code")
                              (info . ,(make-hasheq `((public . ,out-public) (private . ,out-private)))))))))]
      [(openai/non-code? resp)
       (make-hasheq `((type . "non-code")))]
      [(openai/server-error resp)
       (make-hasheq `((type . "server-error")))]))

  (map gen-one-result loresps))

;; run-test-suite :: test-suite * SExp *  Boolean ->  a hash table of count of types of result, or 'fail if there's a timeout
(define (run-test-suite test-suite fun-def-sexp #:include-failure-details? incl-fail?)
  (define local-eval (make-evaluator 'racket 'empty))
  (local-eval '(require racket))
  (local-eval '(require rackunit))
  (local-eval '(require json))
  (local-eval '(require rackunit/private/check-info))
  ;; for pretty-info-value; see
  ;; https://discord.com/channels/571040468092321801/667522224823205889/1120492137566457958

  (define e (engine (lambda (_)
                      (local-eval fun-def-sexp)
                      (let ([results (local-eval `(run-test ,test-suite))])
                        (make-hash `((count .   ,(length results))
                                     (success . ,(length (filter (local-eval 'test-success?) results)))
                                     (failure . ,(length (filter (local-eval 'test-failure?) results)))
                                     (error   . ,(length (filter (local-eval 'test-error?)   results)))
                                     (outcomes . ,(map (lambda (result)
                                                         (cond
                                                           [((local-eval 'test-success?) result) (make-hash `((outcome . "success")))]
                                                           [((local-eval 'test-failure?) result) (make-hash
                                                                                                  `((outcome . "failure")
                                                                                                    (details . ,(if incl-fail?
                                                                                                                    (local-eval
                                                                                                                     `(with-output-to-string
                                                                                                                        (lambda ()
                                                                                                                          (print-info-value
                                                                                                                           (check-info-value
                                                                                                                            (first
                                                                                                                             (filter
                                                                                                                              (lambda (c) (eq? 'actual (check-info-name c)))
                                                                                                                              (exn:test:check-stack (test-failure-result ,result)))))))))
                                                                                                                    (local-eval `(json-null))))))]
                                                           [((local-eval 'test-error?)   result) (make-hash `((outcome . "error")))]))
                                                       results))
                                     ))))))
  (define o (engine-run 100 e))
  (if o
      (engine-result e)
      'fail))

