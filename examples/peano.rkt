#lang formica
(require rackunit)

;;;====================================================
;;; Definition of Peano axioms 
;;;====================================================

;; Declare these functions to be formal
(define-formal 1+ Add Mul Sub Div)

;; Peano axioms
(define//. Peano
  ; definition of Peano numerals
  0 --> 0
  (? number? n) --> (1+ (- n 1))
  
  ; addition
  (Add x 0) --> x
  (Add x (1+ y)) --> (1+ (Add x y))
  
  ; multiplication
  (Mul _ 0) --> 0
  (Mul x (1+ y)) --> (Add x (Mul x y))
  
  ; subtitution
  (Sub x 0) --> x
  (Sub 0 _) --> 0
  (Sub (1+ x) (1+ y)) --> (Sub x y)
  
  ; division (quotient ceiling)
  (Div 0 _) --> 0
  (Div (1+ x) (1+ y)) --> (1+ (Div (Sub x y) (1+ y))))


;; interpretation of Peano numerals as regular numbers
(define//. interprete
  (1+ (? number? n)) --> (+ 1 n))

;; Calculation in terms of Peano numerals
(define calculate (compose interprete Peano))


(test-case
 "Pano arythmetic"
  (check-equal? (Peano 1)  (1+ 0))
  (check-equal? (Peano 3)  (1+ (1+ (1+ 0))))
  (check-equal? (Peano (Add 2 3))  (1+ (1+ (1+ (1+ (1+ 0))))))

  (check-equal? (interprete 0)  0)
  (check-equal? (interprete (1+ 0))  1)
  (check-equal? (interprete (1+ (1+ (1+ 0))))  3)


  (check-equal? (calculate (Add 2 2))  4)
  (check-equal? (calculate (Add 3 0))  3)
  (check-equal? (calculate (Add 0 0))  0)
  (check-equal? (calculate (Mul 3 2))  6)
  (check-equal? (calculate (Mul 2 3))  6)
  (check-equal? (calculate (Mul 0 3))  0)
  (check-equal? (calculate (Sub 2 2))  0)
  (check-equal? (calculate (Sub 3 2))  1)
  (check-equal? (calculate (Sub 1 2))  0)
  (check-equal? (calculate (Div 4 2))  2)
  (check-equal? (calculate (Div 2 4))  1)
  (check-equal? (calculate (Div 15 3))  5)
  (check-equal? (calculate (Div 15 4))  4)
  (check-equal? (calculate (Div 16 3))  6)
  (check-equal? (calculate (Mul (Add 2 3) (Sub 4 2)))  10))