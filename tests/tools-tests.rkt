#lang racket/base
(require "../private/tools/tools.rkt"
         "test-utils.rkt"
         racket/list)

(test-case
 "implication tests"
 (check-true (procedure? ==>) "Implication is a function")
 (check-equal? (procedure-arity ==>) 2 "Implication is binary")
 (check-equal? (map ==> '(#f #f #t #t) '(#f #t #f #t)) '(#t #t #f #t) "Value table")
 (check-equal? (==> #f (/ 1 0)) #t "Implication is a form")
 (check-exn exn:fail? (λ () (==> #t (/ 1 0))) "Implication is a form"))

(test-case
 "or tests"
 (check-true (procedure? or) "or is a function")
 (check-equal? (procedure-arity or) (arity-at-least 2) "or is at least binary")
 (check-equal? (map or '(#f #f #t #t) '(#f #t #f #t)) '(#f #t #t #t) "Value table")
 (check-equal? (or #t (/ 1 0)) #t "or is a form")
 (check-exn exn:fail? (λ () (or #f (/ 1 0))) "or is a form"))

(test-case
 "and tests"
 (check-true (procedure? and) "and is a function")
 (check-equal? (procedure-arity  and) (arity-at-least 2) "and is at least binary")
 (check-equal? (map and '(#f #f #t #t) '(#f #t #f #t)) '(#f #f #f #t) "Value table")
 (check-equal? (and #f (/ 1 0)) #f "and is a form")
 (check-exn exn:fail? (λ () (and #t (/ 1 0))) "and is a form"))

(test-case
 "xor tests"
 (check-true (procedure? xor) "xor is a function")
 (check-equal? (procedure-arity xor) 2 "xor is at least binary")
 (check-equal? (map xor '(#f #f #t #t) '(#f #t #f #t)) '(#f #t #t #f) "Value table"))

(test-case
 "almost-equal? tests"
 (tolerance 5e-16)
 (check-true (almost-equal? 1 1))
 (check-true (almost-equal? 1. 1.))
 (check-true (almost-equal? 0 0))
 (check-true (almost-equal? 0. 0.))
 (check-true (almost-equal? 1. 1.+0i 1+0i 1.+0i 1+1e-16i (+ 1 1e-16)))
 (check-true (almost-equal? 0. 0.+0i 0+0i 0.+0i 0+1e-16i 1e-16))
 (check-true (almost-equal? 1 1.0))
 (check-false (almost-equal? 1 'x))
 (check-false (almost-equal? 1 2))
 (check-false (almost-equal? 1 (+ 1 1e-14)))
 (check-true (almost-equal? 1 (+ 1 1e-17)))
 (check-true (almost-equal? 1+2i 1+2i))
 (check-true (almost-equal? 1+2i 1.0+2.0i))
 (check-true (almost-equal? 1+2i 1.0+2.0000000000000005i))
 (check-false (almost-equal? 1+2i 1.0+2.00000000000001i))
 (check-true (almost-equal? 'a 'a))
 (check-true (almost-equal? '(a (b c)) '(a (b c))))
 (check-true (almost-equal? '(1 (2 3)) '(1 (2 3.000000000000001))))
 (check-false (almost-equal? '(1 (2 3)) '(1 (2 3.00000000000001)))))

(test-case
 "tolerance tests"
(parameterize ([tolerance 0.01])
    (check-true (almost-equal? 1 1.001))
    (check-true (almost-equal? 10 10.01))
    (check-true (almost-equal? 1e23 1.001e23))
    (check-true (almost-equal? 1e-23 1.001e-23))))

     