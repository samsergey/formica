#lang racket/base
(require "../testing.rkt"
         "../private/tools/arity.rkt"
         "test-utils.rkt")

(test
 
 "equality tests"
 2 ==> 2
 (map add1 '(1 2 3)) ==> '(2 3 4)
 (display "Testing tests") ==> (void)
 (check-exn exn? (λ () (test 2 ==> 3)))

 "truth tests"
  2
  (< 2 3)
  (check-true (< 2 3))
  (check-exn exn? (λ () (test (< 3 2))))

 "exeption tests"
 (/ 0) =error=> exn:fail?
 (+ 'x 'y) =error=> exn:fail:contract?
 (check-exn exn? (λ () (test (/ 0 1) =error=> exn:fail?)))

 "using rackunit checks"
 (check < 2 3)
 (check-false (< 3 2))
 (check-exn exn:fail? (λ () (/ 0)))
 (check-exn exn? (λ () (test (check-equal? 2 3)))))