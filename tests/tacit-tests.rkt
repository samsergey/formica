#lang racket/base
(require "../tacit.rkt"
         "../rewrite.rkt"
         "test-utils.rkt"
         rackunit)


(define/c (m f)
  (/. (cons h t) --> (cons (f h) (m f t))
      '() x ... --> (? (andmap null? x)) '()
      l1 ... --> (cons (apply f (m car l1)) 
                          (apply m f (m cdr l1)))))

(test-case
 "general tests"
  (check-equal? (procedure-arity m) (arity-at-least 2))
  (check-equal? (m u '(1 2 3)) '((u 1) (u 2) (u 3)))
  (check-equal? (m b '(1 2 3) '(a b c)) '((b 1 a) (b 2 b) (b 3 c))))

(define/c (F1) (/. y --> 4 5 --> 6))
(define/c (F2 x) (/. y --> x 5 --> 6))
(define/c (F3 x) (/. y --> x 5 7 --> 6))
(define/c (F3* x) (/. y --> x 5 7 y ... --> 6))

(define/c (G1) (//. y --> 4 5 --> 6))
(define/c (G2 x) (//. y --> x 5 --> 6))
(define/c (G3 x) (//. y --> x 5 7 --> 6))
(define/c (G3* x) (//. y --> x 5 7 y ... --> 6))

(define/c (T1) (//. y -->. 4 5 -->. 6))
(define/c (T2 x) (//. y -->. x 5 -->. 6))
(define/c (T3 x) (//. y -->. x 5 7 -->. 6))
(define/c (T3* x) (//. y -->. x 5 7 y ... -->. 6))

(define/c (S1) (//. y --> 4 5 -->. 6))
(define/c (S2 x) (//. y --> x 5 -->. 6))
(define/c (S3 x) (//. y --> x 5 7 -->. 6))
(define/c (S3* x) (//. y --> x 5 7 y ... -->. 6))
(define/c (S3*? x) (//. y --> x 5 7 y ... -->. (? #t) 6))
(define/c (S3*=> x) (//. y --> x 5 7 y ... -->. (=> values) 6))

(define/c (f0) (t 'a 'b))
(define/c (f0f) (t 'a 'b 'c))
(define/c (f1 x) (t (b 'a x)))
(define/c (f2 x) (t 'a 'b))
(define/c (f3 x) (v*))
(define/c (f4 x) (v* 'a))
(define/c (f5 x) (v* 'a 'b))
(define/c (f6 x) (v* 'a 'b 'c))

(test-case
 "arity tests"
  (check-equal? (procedure-arity F1) 1)
  (check-equal? (procedure-arity F2) 2)
  (check-equal? (procedure-arity F3) '(2 3))
  (check-equal? (procedure-arity F3*) `(2 ,(arity-at-least 4)))
  (check-equal? (procedure-arity G1) 1)
  (check-equal? (procedure-arity G2) 2)
  (check-equal? (procedure-arity G3) '(2 3))
  (check-equal? (procedure-arity G3*) `(2 ,(arity-at-least 4)))
  (check-equal? (procedure-arity T1) 1)
  (check-equal? (procedure-arity T2) 2)
  (check-equal? (procedure-arity T3) '(2 3))
  (check-equal? (procedure-arity T3*) `(2 ,(arity-at-least 4)))
  (check-equal? (procedure-arity S1) 1)
  (check-equal? (procedure-arity S2) 2)
  (check-equal? (procedure-arity S3) '(2 3))
  (check-equal? (procedure-arity S3*) `(2 ,(arity-at-least 4)))
  (check-equal? (procedure-arity S3*?) `(2 ,(arity-at-least 4)))
  (check-equal? (procedure-arity S3*=>) `(2 ,(arity-at-least 4)))
  
  (check-equal? (procedure-arity f0) 1)
  (check-equal? (f0 'x) (t 'a 'b 'x))
  (check-equal? (procedure-arity f0f) 0)
  (check-equal? (f0f) (t 'a 'b 'c))
  (check-equal? (procedure-arity f1) 3)
  (check-equal? (f1 'x 'y 'z) (t (b 'a 'x) 'y 'z))
  (check-equal? (procedure-arity f2) 2)
  (check-equal? (f2 'x 'y) (t 'a 'b 'y))
  (check-equal? (procedure-arity f3) (list 2 3 (arity-at-least 4)))
  (check-equal? (f3 'x 'y) (v* 'y))
  (check-equal? (f3 'x 'y 'z) (v* 'y 'z))
  (check-equal? (f3 'x 'y 'z 't 'w) (v* 'y 'z 't 'w))
  (check-equal? (procedure-arity f4) (list 1 2 (arity-at-least 3)))
  (check-equal? (f4 'x) (v* 'a))
  (check-equal? (f3 'x 'y) (v* 'y))
  (check-equal? (f3 'x 'y 'z 't) (v* 'y 'z 't))
  (check-equal? (procedure-arity f5) (list 1 (arity-at-least 2)))
  (check-equal? (f5 'x) (v* 'a 'b))
  (check-equal? (f5 'x 'y 'z 't) (v* 'a 'b 'y 'z 't))
  (check-equal? (procedure-arity f6) (arity-at-least 1))
  (check-equal? (f6 'x 'y) (v* 'a 'b 'c 'y)))



