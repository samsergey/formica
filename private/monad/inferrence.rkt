#lang racket/base
;;______________________________________________________________
;;                   ______ 
;;                  (  //   _____ ____   .  __  __
;;                   ~//~ ((_)// // / / // ((_ ((_/_
;;                 (_//
;;..............................................................
;;; monadic type inference
;;;===============================================================================
(require "base.rkt"
         "../types/types.rkt"
         "sequential-monads.rkt"
         "../tools/curry.rkt"
         "../../rewrite.rkt")
(require racket/promise
         racket/sequence
         racket/stream
         racket/math
         racket/list)

(provide Inferred
         using-monad*)

(define monad-types (make-parameter (list (cons Any Id))))

(using-monad List)
(define inferred-return (make-parameter return))
(define inferred-mzero (make-parameter mzero))
(define inferred-mplus (make-parameter mplus))
(define inferred-failure (make-parameter failure))

(define-monad Inferred
  #:return (λ x (apply (inferred-return) x))
  #:bind (λ (m f)
           (define M (cdr (assoc m (monad-types) (λ (x T) (is x T)))))           
           (using M 
             (parameterize 
                 ([inferred-return return]
                  [inferred-mzero (if (monad-zero? M) mzero 'undefined)]
                  [inferred-mplus (if (monad-plus? M) mplus 'undefined)])
               (bind m >>= f))))
  #:mplus (λ (x y) ((inferred-mplus) x y))
  #:mzero (delay/name (inferred-mzero))
  #:failure (λ (x) ((inferred-failure) x)))

(define using-monad*
  (case-lambda
    [() (using-monad)]
    [(M) (using-monad M)]
    [(M . Ms)  (monad-types
                (foldr (λ (m res) (cons (cons (using m type) m) res))
                       (list (cons Any Id))
                       (cons M Ms)))
               (using-monad Inferred)]))