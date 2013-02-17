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
         (only-in "../../tools.rkt" argmap)
         (only-in "../../formal.rkt" formal? n/f-list?)
         (only-in "../../types.rkt" check-result check-argument \\)
         racket/set
         racket/list
         racket/contract
         racket/generator
         racket/stream
         racket/sequence
         unstable/contract)

(provide 
 amb
 !
 zip
 (contract-out 
  ; Monoid
  (Monoid (->* (#:return (->* () #:rest list? listable?)
                  #:mplus (-> listable? listable? listable?)) 
                 (#:map (-> (-> any/c listable?) listable? listable?)
                  #:type contract?) monad-plus?))
  (mplus-map (-> (-> any/c listable?) listable? any/c))
  #;(zip (->* ((\\ sequence? formal?)) #:rest (listof (\\ sequence? formal?)) (sequence/c list?)))
  ; List monad
  (List monad-plus?)  
  (concatenate (->* () #:rest (listof listable?) n/f-list?))
  (concat-map (-> (-> any/c n/f-list?) listable? n/f-list?))
  ; Stream monad
  (Stream monad-plus?)
  (stream-concat-map (-> (-> any/c stream?) listable? stream?)) 
  (stream-concatenate (-> listable? listable? stream?))  
  (stream-take (-> stream? (and/c integer? (>/c 0)) list?))
  ; Amb monad
  (Amb monad-plus?)
  (amb-union-map (-> (-> any/c stream?) listable? stream?))
  (amb-union (-> listable? listable? stream?))))

;;;===============================================================================
;;; general helper functions
;;;===============================================================================
;; the tool for parallel sequencing in theaccumulating monads.
(define (zip . x)
  (define res (in-values-sequence (apply in-parallel x)))
  (if (andmap n/f-list? x) 
     (sequence->list res)
     res))

;;;===============================================================================
;;; Monoid
;;;===============================================================================
(define (mplus-map f m)
  (for/fold ([r mzero]) ([x m])
     (let ([fx (f x)])
       (mplus (f x) r))))

(define (Monoid #:return ret #:map (app-map mplus-map)  #:mplus app #:type (T listable?))
  (monad
   #:type T
   #:return ret
   #:bind (λ (m f) (app-map f m))
   #:mzero (ret)
   #:mplus app
   #:failure (λ (_) mzero)))

;;;===============================================================================
;;; List monad
;;;===============================================================================
(define (concat-map f lst)
  (for*/list ([x (in-list lst)] [fx (in-list (f x))]) fx))

(define concatenate (argmap append sequence->list))

(define-monad List 
  (Monoid
   #:type n/f-list?
   #:return list
   #:map append-map
   #:mplus append))

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
  (Monoid
   #:type listable?
   #:return make-stream
   #:map stream-concat-map
   #:mplus stream-concatenate))

(define (stream-take s n)
  (for/list ([i (in-range n)]
             [x (in-stream s)]) x))

(define ! stream->list)

                     

;;;===============================================================================
;;; Amb monad
;;;===============================================================================
(define-syntax amb
  (syntax-id-rules ()
    [(amb) (stream)]
    [(amb x) (stream x)]
    [(amb x y ...) (amb-union (stream x) (amb y ...))]
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
  (Monoid
   #:return amb
   #:map amb-union-map
   #:mplus amb-union))
