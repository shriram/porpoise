#lang web-server

(require racket/date)
(require "problems.rkt")
(require "config.rkt")

(provide LOG)

(provide NO-USER NO-PROBLEM)

(define NO-USER "no-user")

(define (LOG #:user user #:problem problem #:context context . as)
  (let ([log-message
         (make-hash `((time    . ,(current-milliseconds))
                    (utime   . ,(date->string (seconds->date (current-seconds)) #t))
                    (user    . ,user)
                    (problem . ,(public-name->private-name problem))
                    (context . ,context)
                    (details . ,as)))]
        [log-file-name (path-maker "LOGS" (string-append user ".txt"))])
    (with-output-to-file log-file-name
      #:exists 'append
      (lambda ()
        (writeln log-message)
        (flush-output)))))
