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
         racket/generator
         racket/stream
         racket/sequence)

(provide 
 (contract-out 
  (List monad-plus?)  
  (Set monad-plus?)
  (Amb monad-plus?)
  (listable? contract?))
 (all-from-out racket/set))

;;;===============================================================================
;;; List monad
;;;===============================================================================
(define listable?
  (flat-named-contract 
   'listable?
   (and/c sequence? (not/c formal?))))

(define-monad List
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
  #:failure (λ (_) null))

;;;===============================================================================
;;; Set monad
;;;===============================================================================
(define-monad Set
  #:return (λ (x) (if (set-empty? x) x (set x)))
  #:bind (λ (m f) 
           (unless (and (sequence? m) (not (formal? m))) 
             (raise-type-error 'bind "sequence" m)) 
           (for*/set ([x m] [fx (f x)]) fx))
  #:mzero (set)
  #:mplus set-union)

;;;===============================================================================
;;; Amb monad
;;;===============================================================================
;; a stream of ambient values
(define amb
  (case-lambda
    ; works inside binding
    [(x) (stream x)]
    ; works once at at input
    [(x . y) (sequence->stream (in-set (apply set (cons x y))))]))

;; ambient binding
(define (amb-bind m f) 
  (define g 
    (generator ()
               ; the set of results
               (define s (set))
               (for* ([x m]
                      [fx (check-result 
                            'bind 
                            (flat-named-contract 'stream? stream?)
                            (f x))])
                 (unless (set-member? s fx) 
                   (set! s (set-add s fx))
                   (yield fx)))
               ; the for-cycle is over
               (yield 'end-of-stream)))
  ; return a stream, produced by the generator
  (sequence->stream (in-producer g 'end-of-stream)))

(define-monad Amb
  #:type listable?
  #:return amb
  #:bind amb-bind
  #:mzero empty-stream
  #:mplus stream-append
  #:failure empty-stream)

