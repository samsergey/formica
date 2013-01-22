#lang formica
;;______________________________________________________________
;;                   ______ 
;;                  (  //   _____ ____   .  __  __
;;                   ~//~ ((_)// // / / // ((_ ((_/_
;;                 (_//
;;..............................................................
;;; monadic type inference
;;;===============================================================================
(require racket/promise)

(define monad-types (make-parameter (list (cons Any Id))))

(using-monad Id)
(define inferred-return (make-parameter return))
(define inferred-mzero (make-parameter 'undefined))
(define inferred-mplus (make-parameter 'undefined))
(define inferred-failure (make-parameter failure))

(define-monad Inferred
  #:return (λ x (apply (inferred-return) x))
  #:bind (λ (m f)
           (define M (cdr (assoc m (monad-types) (λ (x T) (is x T)))))
           (parameterize ([inferred-return (using M return)]
                          [inferred-mzero (if (monad-zero? M) 
                                              (using M mzero)
                                              'undefined)]
                          [inferred-mplus (if (monad-plus? M) 
                                              (using M mplus)
                                              'undefined)])
             (apply (using M bind) m (list f))))
  #:mplus (λ (x y) ((inferred-mplus) x y))
  #:mzero (delay/name (inferred-mzero))
  #:failure (λ (x) ((inferred-failure) x)))

(define (using-monad* M . Ms)
  (monad-types
   (foldr (λ (m res) (cons (cons (using m type) m) res))
          (list (cons Any Id))
          (cons M Ms)))
  (using-monad Inferred))

(require formica/examples/Maybe)
(using-monad* Stream List (Maybe Int) )

(bind 7 >>= (lift (* 2)) >>= (lift sqr))
(bind (Just 7) >>= (lift (* 2)) >>= (lift sqr))
(bind (list 7) >>= (lift (* 2)) >>= (lift sqr))
(stream-first (bind (in-value 7) >>= (lift (* 2)) >>= (lift sqr)))

(define (isqrt x)
  (let next ([s 0] [r 0])
    (cond
      [(> s x) mzero]
      [(= s x) (mplus (return r) (return (- r)))]
      [else (next (+ s (* 2 r) 1) (+ 1 r))])))

(bind (Just 16) >>= isqrt >>= isqrt)
(bind (list 16) >>= isqrt)
(bind (range 100) >>= isqrt)
;(stream->list (bind (in-range 10) >>= isqrt))

(using Stream (is (return 6) type))