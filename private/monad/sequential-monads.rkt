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
 scons
 (contract-out 
  (concatenate (->* () #:rest (listof listable?) list?))
  (concat-map (-> (-> any/c any/c) listable? list?))
  (List monad-plus?)  
  (set-union-map (-> (-> any/c any/c) listable? set?))
  (Set monad-plus?)
  (stream-concat-map (-> (-> any/c any/c) listable? stream?)) 
  (stream-concatenate (-> listable? listable? stream?))  
  (stream-take (-> stream? (and/c integer? (>/c 0)) list?))
  (stream->pair (-> (and/c stream? (not/c stream-empty?)) (cons/c any/c stream?)))
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
;;; Sequence monad
;;;===============================================================================
(define (Sequence #:return ret #:map app-map  #:append app)
  (monad
   #:type listable?
   #:return ret
   #:bind (λ (m f) (app-map f m))
   #:mzero (ret)
   #:mplus app
   #:failure (λ (_) mzero)))

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
  (Sequence
   #:return list
   #:map concat-map
   #:append concatenate))

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
  (Sequence
   #:return set
   #:map set-union-map
   #:append set-union))

;;;===============================================================================
;;; Stream monad
;;;===============================================================================

(define make-stream
  (case-lambda
    [() empty-stream]
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
  (Sequence
   #:return make-stream
   #:map stream-concat-map
   #:append stream-concatenate))

(define (stream->pair s)
  (cons (stream-first s) (stream-rest s)))

(define (stream-take s n)
  (for/list ([i (in-range n)]
             [x (in-stream s)]) x))

(define-match-expander scons
  (syntax-rules ()
    [(scons x y) (and (? stream?) 
                      (not (? stream-empty?))
                      (app stream-first x)
                      (app stream-rest y))]))
                      

;;;===============================================================================
;;; Amb monad
;;;===============================================================================
(define-syntax amb
  (syntax-id-rules ()
    [(amb x ...) (stream x ...)]
    ; works once at at input
    [amb (procedure-rename 
          (case-lambda 
            [() empty-stream]
            [(x) (stream x)]
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
  (Sequence
   #:return amb
   #:map amb-union-map
   #:append amb-union))