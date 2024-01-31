#lang web-server

(provide path-maker)

(define (path-maker . rest)
  (apply build-path
         (find-system-path 'home-dir)
         "Desktop" "r" "sk" "porpoise"
         rest))
