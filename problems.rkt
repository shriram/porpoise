#lang web-server

(require racket/serialize)

(require "config.rkt")

(provide load-problems get-problem [struct-out problem] global-problem-list public-name->private-name initialize-problem-list NO-PROBLEM)

;; Format defined in problems.sexp

(serializable-struct problem (private-name public-name synthesized-name raw-public-test-suite public-test-suite private-test-suite bad-impl) #:transparent)

(define problem-set-directory "problem-sets")

(define problems-file-name
  (let ([env (getenv "PORPOISE_PROBLEMS")])
    (if env
        (let ([f (path-maker problem-set-directory env)])
          (if (file-exists? f)
              f
              (error 'problems-file-name "The problem set file `~a` was not found in the `~a` directory." env problem-set-directory)))
        (error 'problems-file-name "You must set the environment variable PORPOISE_PROBLEMS to select the problem set. Choose from the files in the `~a` directory." problem-set-directory))))

(define global-problem-list 'undefined:global-problem-list)

(define (initialize-problem-list)
  (set! global-problem-list (load-problems problems-file-name)))

(define NO-PROBLEM "no-problem")

(define next-problem-name
  (let ([next-unused 0])
    (lambda ()
      (begin0 (string-append "Problem " (number->string next-unused))
        (set! next-unused (add1 next-unused))))))

(define (public-name->private-name pub-name)
  (define (loop problems)
    (cond
      [(empty? problems) (error 'public-name->private-name "name not found: ~s" pub-name)]
      [(cons? problems)
       (if (string=? (problem-public-name (first problems)) pub-name)
           (problem-private-name (first problems))
           (loop (rest problems)))]))
  (if (string=? pub-name NO-PROBLEM)
      pub-name
      (loop global-problem-list)))

(define (gen-test-suite fun-name test-spec)
  (when (null? test-spec)
    (error 'gen-test-suite "empty test spec"))
  `(test-suite "Dummy"
               ,@(map (lambda (a-test)
                        (match a-test
                          [(list pred result inputs ...)
                           `(test-case "Dummy" (,pred (,fun-name ,@inputs) ,result))]
                          [_
                           (error 'gen-test-suite "~s doesn't match" a-test)]))
                      test-spec)))

(define (load-problems fn)
  (with-input-from-file fn
    (lambda ()
      (let loop ()
        (let* ([e (read)]
               [get (lambda (tag [if-not-found-proc? #f])
                      (cond
                        [(assoc tag e) => (lambda (p) (second p))]
                        [if-not-found-proc? (if-not-found-proc?)]
                        [else (error 'load-problems "tag ~s not found in ~s" tag e)]))])
          (if (eof-object? e)
              empty
              (let ([fun-name (get 'synthesized-name)])
                (cons (problem (get 'private-name)
                               (get 'public-name (lambda () (string-append (next-problem-name) ": " (symbol->string fun-name))))
                               fun-name
                               (get 'public-test-suite)
                               (gen-test-suite fun-name (get 'public-test-suite))
                               (gen-test-suite fun-name (get 'private-test-suite))
                               (get 'bad-impl (λ () #f)))
                      (loop)))))))))

(define (get-problem pname plist)
  (let ([r (filter (lambda (p) (string=? (problem-public-name p) pname)) plist)])
    (cond
      [(empty? r) (error 'get-problem "no such problem ~s in ~s" pname plist)]
      [(empty? (rest r)) (first r)]
      [(cons? (rest r)) (error 'get-problem "multiple problems named ~s in ~s" pname plist)])))
