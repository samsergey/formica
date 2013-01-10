#lang racket/base
;;______________________________________________________________
;;                   ______ 
;;                  (  //   _____ ____   .  __  __
;;                   ~//~ ((_)// // / / // ((_ ((_/_
;;                 (_//
;;..............................................................
;; Provides basic tools to work with monads.
;;==============================================================
(require racket/match
         "base.rkt"
         (only-in "../../formal.rkt" formal? n/f-list?)
         (only-in "../../types.rkt" check-result)
         racket/set
         racket/contract
         unstable/contract)

(provide 
 (contract-out 
  (List monad-plus?)  
  (Set monad-plus?)
  (listable? contract?)
  (zip (->* (listable?) #:rest (listof listable?) (sequence/c list?))))
 (all-from-out racket/set))

;;;===============================================================================
;;; helper functions
;;;===============================================================================
(define listable?
  (flat-named-contract 
   'listable?
   (and/c sequence? (not/c formal?))))

(define (concat-map f lst)
  (for*/list 
      ([x lst] 
       [fx (in-list (check-result 
                     'bind 
                     (flat-named-contract 'n/f-list? n/f-list?)
                     (f x)))])
    fx))

;; the tool for parallel sequencing in the Accumulating monads.
(define zip (compose in-values-sequence in-parallel))

;;;===============================================================================
;;; List monad
;;;===============================================================================
(define-monad List
  #:type listable?
  #:return list
  #:bind (λ (m f) (concat-map f m))
  #:mzero null
  #:mplus append
  #:failure (λ (_) null))

;;;===============================================================================
;;; Set monad
;;;===============================================================================
(define-monad Set
  #:type listable?
  #:return (λ (x) (if (and (set? x) (set-empty? x))
                      x 
                      (set x)))
  #:bind (λ (m f) 
           (for*/set ([x m] [fx (check-result 
                                 'bind 
                                 (flat-named-contract 'set? set?)
                                 (f x))]) 
                     fx))
  #:mzero (set)
  #:mplus set-union
  #:failure (λ (_) (set)))

