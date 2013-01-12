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
         (only-in "../../types.rkt" check-result check-argument)
         racket/set
         racket/contract
         racket/generator
         racket/stream
         racket/sequence
         unstable/contract)

(provide 
 (contract-out 
  (List monad-plus?)  
  (Set monad-plus?)
  (lazy-List monad-plus?)
  (lazy-Set monad-plus?)
  (listable? contract?)
  (zip (->* (listable?) #:rest (listof listable?) (sequence/c list?))))
 (all-from-out racket/set
               racket/stream))

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
  #:return set
  #;(λ (x) (if (and (set? x) (set-empty? x))
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

;;;===============================================================================
;;; lazy-List monad
;;;===============================================================================
;; a stream of ambient values
(define lazy-list
  (case-lambda
    ; works inside binding
    [(x) (stream x)]
    ; works once at the input
    [(x y . z) (lazy-append (stream x y) z)]))

;; ambient binding
(define (lazy-bind m f) 
  (define g 
    (generator ()
               (for* ([x m]
                      [fx (check-result 
                           'bind 
                           (flat-named-contract 'stream? (and/c stream? (not/c formal?)))
                           (f x))])
                 (yield fx))
               ; the for-cycle is over
               (yield 'end-of-stream)))
  ; return a stream, produced by the generator
  (sequence->stream (in-producer g 'end-of-stream)))

(define (lazy-append s1 s2) 
  (define g 
    (generator ()
               (check-argument 'mplus (flat-named-contract 'listable? listable?) s1)
               (for ([x s1]) (yield x))
               ; the first for-cycle is over
               (check-argument 'mplus (flat-named-contract 'listable? listable?) s2)
               (for ([x s2]) (yield x))
               (yield 'end-of-stream)))
  ; return a stream, produced by the generator
  (sequence->stream (in-producer g 'end-of-stream)))

(define-monad lazy-List
  #:type listable?
  #:return lazy-list
  #:bind lazy-bind
  #:mzero empty-stream
  #:mplus lazy-append
  #:failure (λ (_) empty-stream))

;;;===============================================================================
;;; lazy-Set monad
;;;===============================================================================
(define lazy-set
  (case-lambda
    ; works inside binding
    [(x) (stream x)]
    ; works once at at input
    [(x . y) (lazy-set-union (stream x) y)]))
     

(define (lazy-set-bind m f) 
  (define g 
    (generator ()
               ; the set of results
               (define s (set))
               (for* ([x m]
                      [fx (check-result 
                           'bind 
                           (flat-named-contract 'stream? (and/c stream? (not/c formal?)))
                           (f x))])
                 (unless (set-member? s fx) 
                   (set! s (set-add s fx))
                   (yield fx)))
               ; the for-cycle is over
               (yield 'end-of-stream)))
  ; return a stream, produced by the generator
  (sequence->stream (in-producer g 'end-of-stream)))

(define (lazy-set-union s1 s2) 
  (define g 
    (generator ()
               ; the set of results
               (define s (set))
               (check-argument 'mplus (flat-named-contract 'listable? listable?) s1)
               (for ([x s1])
                 (unless (set-member? s x) 
                   (set! s (set-add s x))
                   (yield x)))
               ; the first for-cycle is over
               (check-argument 'mplus (flat-named-contract 'listable? listable?) s2)
               (for ([x s2])
                 (unless (set-member? s x) 
                   (set! s (set-add s x))
                   (yield x)))
               (yield 'end-of-stream)))
  ; return a stream, produced by the generator
  (sequence->stream (in-producer g 'end-of-stream)))

(define-monad lazy-Set
  #:type listable?
  #:return lazy-set
  #:bind lazy-set-bind
  #:mzero empty-stream
  #:mplus lazy-set-union
  #:failure (λ (_) empty-stream))
