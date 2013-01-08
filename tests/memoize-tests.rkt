#lang racket/base
(require "../memoize.rkt"
         "test-utils.rkt"
         "../tools.rkt"
         "../rewrite.rkt")

(test-case
 "simple memoization tests"
 (define flag 1)
 (define/memo (F x) (set! flag x) flag)
 
 (check-true (memoized? F))
 (check-false (memoized? +))
 (check-false (memoized? 5))
 
 (check-equal? flag 1)
 (F 2)
 (check-equal? flag 2)
 (F 3)
 (check-equal? flag 3)
 (F 2)
 (check-equal? flag 3)
 (F +)
 (check-equal? flag +)
 (F 2)
 (check-equal? flag +)
 (F '(1 2 3))
 (check-equal? flag '(1 2 3))
 (F +)
 (check-equal? flag '(1 2 3))
 
 (define G (memoized +))
 (check-true (memoized? G))
 (check-equal? (G 2 3) (+ 2 3) "Functional equivalence")
 
 (check-equal? (memoized G) G "Double memoization")
 
 (define/memo (f x y . z) (set! flag x) (list* x y z))
 (check-equal? (procedure-arity f) (arity-at-least 2))
 (check-equal? (f 1 2) '(1 2))
 (check-equal? flag 1)
 (check-equal? (f 2 3 4) '(2 3 4))
 (check-equal? flag 2)
 (check-equal? (f 1 2) '(1 2))
 (check-equal? flag 2)

 (define/memo (g . x) (set! flag x) (list* x))
 (check-equal? (procedure-arity g) (arity-at-least 0))
 (check-equal? (g 1 2) '(1 2))
 (check-equal? flag '(1 2))
 (check-equal? (g 1 2 3) '(1 2 3))
 (check-equal? flag '(1 2 3))
 (check-equal? (g 1 2) '(1 2))
 (check-equal? flag '(1 2 3))
 
 (define/memo H sqrt)
 (check-equal? (H 4) 2)
 (check-true (memoized? H))
 
 (define/memo L (/. 5 --> (begin (set! flag 5) 6) 
                    7 --> (begin (set! flag 7) 8)))
 (L 5)
 (check-equal? flag 5)
 (L 7)
 (check-equal? flag 7)
 (L 5)
 (check-equal? flag 7)
 
 (define (kons x y) 
   (set! flag (cons x y))
   (cons x y))
 
 (define/memo K (flipped kons))
 (check-true (memoized? K))
 (check-equal? (K 1 4) (cons 4 1))
 (check-equal? flag (cons 4 1))
 (check-equal? (K 2 3) (cons 3 2))
 (check-equal? flag (cons 3 2))
 (K 1 4)
 (check-equal? flag (cons 3 2))
 
 (define/memo/c (M x) (kons (+ 2 x)))
 (check-equal? (M 4 5) (cons 6 5))
 (check-equal? flag (cons 6 5))
 (check-equal? (M 1 2) (cons 3 2))
 (check-equal? flag (cons 3 2))
 (M 4 5)
 (check-equal? flag (cons 3 2)))
