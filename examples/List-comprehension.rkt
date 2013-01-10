#lang formica
(require rackunit)
(using-monad List)
;;;===============================================================================
;;; Examples
;;;===============================================================================
(define-formal f)

(test-case
 "simple list comprehension"
 (check-equal? (collect (list a b) (a <- '(1 2 3)) (b <- '(a b c))) 
               '((1 a) (1 b) (1 c) (2 a) (2 b) (2 c) (3 a) (3 b) (3 c)))
 
 (check-equal? (collect (* x x) (x <- (range 1 10)) (odd? x))  
               '(1 9 25 49 81))
 
 (check-equal? (collect (* x x) (x <- 10) (odd? x))  
               '(1 9 25 49 81)))

(test-case
 "a list of Pythagorean triangles"
 (check-equal? (collect (list a b c)
                 [ a <- (range 1 20) ]
                 [ b <- (range 1 a) ]
                 [ c <- (range 1 b) ]
                 (= (sqr a) (+ (sqr b) (sqr c)))) 
               '((5 4 3) (10 8 6) (13 12 5) (15 12 9) (17 15 8)))
 
 (check-equal? (collect (list c a b)
                 [a <- (range 1 20)]
                 [b <- (range 1 a)]
                 [c <-: (sqrt (+ (sqr a) (sqr b)))]
                 (integer? c))
               '((5 4 3) (10 8 6) (13 12 5) (15 12 9) (17 15 8) (20 16 12))))