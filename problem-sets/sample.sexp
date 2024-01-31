;; Format:
;; - private-name: String = private problem descriptor
;; - public-name: String = publicly-visible problem name
;; - synthesized-name: Symbol = what name the tests use
;; - public-test-suite: TestSuite = test suite shown to the student
;; - private-test-suite: TestSuite = test suite used for testing synthesized code (public tests will be appended automatically)

;; A TestSuite is a list of tests. Each test is a list of
;; - a checking function
;; - the expected result
;; - each of the arguments [note: result goes BEFORE the arguments, which can be as many as needed]
;; from which the call is automatically synthesized: e.g.,
;; (check-equal? 2 '(1 2 3)) --> (check-equal? (rf '(1 2 3)) 2)

((private-name "addition")
 (synthesized-name p0)
 (public-test-suite
  ((check-equal? 5 2 3)
   (check-equal? 5 3 2)
   (check-equal? 5 0 5)
   (check-equal? 5 7 -2)
   (check-equal? -3 -1 -2)
   (check-equal? 0 -2 2)))
 (private-test-suite
  ((check-equal? 5 1 4)
   (check-equal? 5 5 0)
   (check-equal? -7 -1 -6)
   (check-equal? -14 -7 -7)))
 (bad-impl
"(define (p0 x y)
  (- x y))"))

((private-name "minimum")
 (synthesized-name p1)
 (public-test-suite
  ((check-equal? 2 2 3)
   (check-equal? 2 3 2)
   (check-equal? 0 0 5)
   (check-equal? -2 7 -2)
   (check-equal? -2 -1 -2)
   (check-equal? -2 -2 2)))
 (private-test-suite
  ((check-equal? 1 1 4)
   (check-equal? 0 5 0)
   (check-equal? -6 -1 -6)
   (check-equal? -7 -7 -7)))
 (bad-impl
"(define (p1 x y)
  (+ x y))"))
