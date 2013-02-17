#lang racket
(require "../monad.rkt"
         "../formal.rkt"
         "../rewrite.rkt"
         "../tools.rkt"
         "../types.rkt"
         rackunit)

(require formica/examples/Maybe)
(define-type m)
(define-monad M
  #:type m?
  #:return m
  #:bind (/. (m x) f --> (f x)))

(using-monad List M (Maybe Int))

(test-case 
 "binding tests"
 (check-equal? (bind 7 >>= (lift (curry * 2)) >>= (lift (+ 1))) 196)
 (check-equal? (bind (Just 7) >>= (lift (curry * 2)) >>= (lift sqr)) (Just 196))
; (check-equal? (bind (list 7) >>= (lift (curry * 2)) >>= (lift sqr)) '(196))
 (check-equal? (bind (m 7) >>= (lift (curry * 2)) >>= (lift sqr)) (m 196))
 (check-equal? (sequence->list (bind (in-value 7) >>= (lift (curry * 2)) >>= (lift sqr))) '(196)))

(test-case 
 "mplus, mzero tests"
 (define (isqrt x)
  (let next ([s 0] [r 0])
    (cond
      [(> s x) mzero]
      [(= s x) (mplus (return r) (return (- r)))]
      [else (next (+ s (* 2 r) 1) (+ 1 r))])))

 (check-equal? (bind (Just 16) >>= isqrt >>= isqrt) (Just 2))
 (check-equal? (bind (list 16) >>= isqrt) '(4 -4))
 (check-equal? (bind (range 100) >>= isqrt)
               '(0 0 1 -1 2 -2 3 -3 4 -4 5 -5 6 -6 7 -7 8 -8 9 -9))
 (check-equal? (stream->list (bind (in-range 100) >>= isqrt))
               '(0 0 1 -1 2 -2 3 -3 4 -4 5 -5 6 -6 7 -7 8 -8 9 -9)))
