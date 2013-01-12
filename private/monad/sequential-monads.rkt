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
         (only-in "../../tools.rkt" fork)
         (only-in "../../formal.rkt" formal? n/f-list?)
         (only-in "../../types.rkt" check-result check-argument)
         racket/set
         racket/contract
         racket/generator
         racket/stream
         racket/sequence
         unstable/contract)

(provide 
 amb
 (contract-out 
  (concatenate (->* () #:rest (listof listable?) list?))
  (concat-map (-> (-> any/c any/c) listable? list?))
  (List monad-plus?)  
  (set-union-map (-> (-> any/c any/c) listable? set?))
  (Set monad-plus?)
  (stream-concat-map (-> (-> any/c any/c) listable? stream?)) 
  (stream-concatenate (-> listable? listable? stream?))
  (Stream monad-plus?)
  (amb-union-map (-> (-> any/c any/c) listable? stream?))
  (amb-union (-> listable? listable? stream?))
  (Amb monad-plus?)
  (listable? contract?)
  (zip (->* (listable?) #:rest (listof listable?) (sequence/c list?))))
 (all-from-out racket/set
               racket/stream))

;;;===============================================================================
;;; general helper functions
;;;===============================================================================
(define listable?
  (flat-named-contract 
   'listable?
   (and/c sequence? (not/c formal?))))


;; the tool for parallel sequencing in the Accumulating monads.
(define zip (compose in-values-sequence in-parallel))

;;;===============================================================================
;;; List monad
;;;===============================================================================
(define (concat-map f lst)
  (for*/list 
      ([x lst] 
       [fx (in-list (check-result 
                     'bind 
                     (flat-named-contract 'n/f-list? n/f-list?)
                     (f x)))])
    fx))

(define concatenate (fork append sequence->list))

(define-monad List
  #:type listable?
  #:return list
  #:bind (λ (m f) (concat-map f m))
  #:mzero null
  #:mplus concatenate
  #:failure (λ (_) null))

;;;===============================================================================
;;; Set monad
;;;===============================================================================
(define (set-union-map f lst)
  (for*/set ([x lst] 
             [fx (check-result 
                  'bind 
                  (flat-named-contract 'set? set?)
                  (f x))]) 
            fx))

(define-monad Set
  #:type listable?
  #:return set
  #:bind (λ (m f) (set-union-map f m))
  #:mzero (set)
  #:mplus set-union
  #:failure (λ (_) (set)))

;;;===============================================================================
;;; Stream monad
;;;===============================================================================

(define make-stream
  (case-lambda
    ; works inside binding
    [(x) (stream x)]
    ; works once at the input
    [(x y . z) (stream-concatenate (stream x y) z)]))

(define (stream-concat-map f m) 
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

(define (stream-concatenate s1 s2) 
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

(define-monad Stream
  #:type listable?
  #:return make-stream
  #:bind (λ (m f) (stream-concat-map f m))
  #:mzero empty-stream
  #:mplus stream-concatenate
  #:failure (λ (_) empty-stream))

;;;===============================================================================
;;; Amb monad
;;;===============================================================================
(define-syntax amb
  (syntax-id-rules ()
    [(amb x ...) (stream x ...)]
    ; works once at at input
    [amb (procedure-rename 
          (case-lambda 
            [(x) (amb x)]
            [(x . y) (amb-union (stream x) y)])
          'amb)]))


(define (amb-union-map f m) 
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

(define (amb-union s1 s2) 
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

(define-monad Amb
  #:type listable?
  #:return amb
  #:bind (λ (m f) (amb-union-map f m))
  #:mzero empty-stream
  #:mplus amb-union
  #:failure (λ (_) empty-stream))