#lang racket
(require "../private/tools/curry.rkt"
         "test-utils.rkt")

(test-case 
 "curried function arity tests"
 (check-equal? (procedure-arity (curry n)) 0)
 (check-equal? (procedure-arity (curry u)) 1)
 (check-equal? (procedure-arity (curry b)) 2)
 (check-equal? (procedure-arity (curry n*)) (arity-at-least 0))
 (check-equal? (procedure-arity (curry u*)) (arity-at-least 1))
 (check-equal? (procedure-arity (curry b*)) (arity-at-least 2))
 (check-equal? (procedure-arity (curry v)) '(1 2 4))
 (check-equal? (procedure-arity (curry v*)) `(1 2 ,(arity-at-least 3)))
 (check-exn exn:fail:contract:arity? (λ () (curry n 1))))

(test-case 
 "functionality tests"
 
 (check-equal? (curried? (curry n)) #t)
 (check-equal? ((curry n)) '(n))
 
 (check-equal? (curried? (curry u)) #t)
 (check-equal? ((curry u) 1) '(u 1))
 
 (check-equal? (procedure-arity (curry b 1)) 1)
 (check-equal? (procedure? (curry b 1 2)) #f)
 
 (check-equal? (procedure-arity (curry n* 1)) (arity-at-least 0))
 (check-equal? ((curry n* 1)) (n* 1))
 (check-equal? ((curry n* 1) 2 3) (n* 1 2 3))
 (check-equal? ((curry n* 1 2 3)) (n* 1 2 3))
 
 (check-equal? (procedure-arity (curry u* 1)) (arity-at-least 0))
 (check-equal? ((curry u* 1)) (u* 1))
 (check-equal? ((curry u* 1) 2 3) (u* 1 2 3))
 (check-equal? ((curry u* 1 2 3)) (u* 1 2 3))
 
 (check-equal? (procedure-arity (curry b* 1)) (arity-at-least 1))
 (check-equal? (procedure-arity (curry b* 1 2)) (arity-at-least 0))
 (check-equal? ((curry b* 1) 2) (b* 1 2))
 (check-equal? ((curry b* 1 2)) (b* 1 2))
 
 (check-equal? (procedure-arity (curry v 1)) '(0 1 3))
 (check-equal? ((curry v 1)) '(v 1))
 (check-equal? ((curry v 1) 2) '(v 1 2))
 (check-exn exn:fail:contract:arity? (λ() ((curry v 1) 2 3)))
 (check-equal? ((curry v 1) 2 3 4) '(v 1 2 3 4))
 
 (check-equal? (procedure-arity (curry v 1 2)) '(0 2))
 (check-equal? ((curry v 1 2)) '(v 1 2))
 (check-exn exn:fail:contract:arity? (λ() ((curry v 1 2) 3)))
 (check-equal? ((curry v 1 2) 3 4) '(v 1 2 3 4))
 
 (check-equal? (procedure-arity (curry v 1 2 3)) 1)
 (check-exn exn:fail:contract:arity? (λ() ((curry v 1 2 3))))
 (check-equal? ((curry v 1 2 3) 4) '(v 1 2 3 4))
 (check-exn exn:fail:contract:arity? (λ() ((curry v 1 2 3) 4 5)))
 
 (check-equal? (procedure-arity (curry v* 1)) `(0 1 ,(arity-at-least 2)))
 (check-equal? ((curry v* 1)) '(v* 1))
 (check-equal? ((curry v* 1) 2) '(v* 1 2))
 (check-equal? ((curry v* 1) 2 3) '(v* 1 2 3))
 
 (check-equal? (procedure-arity (curry v* 1 2)) `(0 ,(arity-at-least 1)))
 (check-equal? (procedure-arity (curry v* 1 2 3)) (arity-at-least 0))
 (check-equal? (procedure-arity (curry v* 1 2 3 4)) (arity-at-least 0)))