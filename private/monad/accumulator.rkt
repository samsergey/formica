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
         racket/contract)

(provide 
 (contract-out 
  (List monad-plus?)  
  (Set monad-plus?)
  (Or monad-plus?)
  (And monad-plus?)
  (listable? contract?))
 (all-from-out racket/set))

;;;===============================================================================
;;; List monad
;;;===============================================================================
(define listable?
  (flat-named-contract 
   'listable?
   (and/c sequence? (not/c formal?))))

(define-monad-plus List
  #:type listable?
  #:return list
  #:bind (λ (m f)            
           (for*/list ([x m] 
                       [fx (check-result 
                            'bind 
                            (flat-named-contract 'n/f-list? n/f-list?)
                            (f x))])
             fx))
  #:mzero null
  #:mplus append
  #:fail (λ (_) null))

;;;===============================================================================
;;; Set monad
;;;===============================================================================
(define-monad-plus Set
  #:return (λ (x) (if (set-empty? x) x (set x)))
  #:bind (λ (m f) 
           (unless (and (sequence? m) (not (formal? m))) 
             (raise-type-error 'bind "sequence" m)) 
           (for*/set ([x m] [fx (f x)]) fx))
  #:mzero (set)
  #:mplus set-union)

;;;===============================================================================
;;; Or monad
;;;===============================================================================
(define-monad-plus Or
  #:return (λ (x) (if (boolean? x) x (in-value x)))
  #:bind (match-lambda*
           [(list #t _)  #t]
           [(list #f f) (f #f)]
           [(list m f) (unless (and (sequence? m) (not (formal? m))) 
                         (raise-type-error 'bind "sequence" m)) 
                       (for/or ([x m]) (f x))])
  #:mzero #f
  #:mplus (λ (a b) (or a b)))

;;;===============================================================================
;;; And monad
;;;===============================================================================
(define-monad-plus And
  #:return (λ (x) (if (boolean? x) x (in-value x)))
  #:bind (match-lambda*
           [(list #f _) #f]
           [(list #t f) (f #t)]
           [(list m f)  (unless (and (sequence? m) (not (formal? m))) 
                          (raise-type-error 'bind "sequence" m)) 
                        (for/and ([x m]) (f x))])
  #:mzero #t
  #:mplus (λ (a b) (and a b)))

