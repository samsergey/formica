#lang racket/base
;;______________________________________________________________
;;                   ______ 
;;                  (  //   _____ ____   .  __  __
;;                   ~//~ ((_)// // / / // ((_ ((_/_
;;                 (_//
;;..............................................................
;; Provides generalized function composition
;;;=============================================================
(require racket/list
         racket/contract
         "arity.rkt"
         "tags.rkt"
         (only-in "functionals.rkt" andf /@))


(provide 
 (contract-out
  (composition (->* (procedure? procedure?) #:rest (listof procedure?) procedure?))
  (∘ (->* (procedure? procedure?) #:rest (listof procedure?) procedure?))
  (greedy? predicate/c)
  (greedy (-> procedure? procedure?)))
 ;; provided for tests
 add-arity
 feed
 nest-arity)

;;=================================================================================
;; helper functions
;;=================================================================================
(define nullary*? (andf variadic? nullary?))

(define (arity-min a)
  (cond 
    [(list? a) (apply min (map arity-min a))]
    [(arity-at-least? a) (if (zero? (arity-at-least-value a)) 
                             0
                             (arity-at-least-value a))]
    [(integer? a) a]))

(define (one-element-list? l)
  (and (pair? l) (null? (cdr l))))

(define (normalized-min-arity f)
  (let [(ar (procedure-arity f))]
    (if (arity-at-least? ar)
        (max 1 (min-arity f))
        (min-arity f))))

(define (maximal-arity f)
  (define ar (procedure-arity f))
  (cond
    [(integer? ar) ar]
    [(arity-at-least? ar) ar]
    [(list ar) (if (ormap arity-at-least? ar)
                   (argmin arity-at-least-value (filter arity-at-least? ar))
                   (apply max ar))]))

(define (nest-min-arity . f)
  (define ar (apply nest-arity f))
  (cond
    [(integer? ar) ar]
    [(arity-at-least? ar) (arity-at-least-value ar)]
    [(list? ar) (apply min (map nest-min-arity ar))]))

;;=================================================================================
;; addition of arities in composition
;;=================================================================================
(define (add-arity a res)
  (let ([res
         (cond
           [(list? a) (map (λ (x) (add-arity x res)) (remove* '(0) a))]
           [(list? res) (add-arity a (arity-min res))]
           [(arity-at-least? a) (arity-at-least (add-arity (max 1 (arity-at-least-value a)) res))]
           [(arity-at-least? res) (if (zero? (arity-at-least-value res))
                                      (add-arity a 0)
                                      (add-arity a (arity-min res)))]
           [(zero? a) #f]
           [(and (integer? a) (integer? res)) (+ res a -1)]
           [else (error (format "Cannot add ~a to ~a" a res))])])
    (if (one-element-list? res) (car res) res)))

;;=================================================================================
;; calculation of the arity of composition
;;=================================================================================
(define nest-arity
  (case-lambda
    [(f) (procedure-arity f)]
    [f  (define fs (reverse f))
        (define f1 (car fs))
        (for/fold ([res (if (nullary*? f1) 1 (procedure-arity f1))]) 
          ([f (cdr fs)] [i (in-naturals 2)])
          (or (add-arity (procedure-arity f) res)
              (error 'nest (format "Cannot nest function with zero arity!\n  function: ~a\n  position: ~a" f i))))]))

;;=================================================================================
;; the generalized composition
;;=================================================================================
;; exportig the function
(define (export  fn)
  (procedure-rename fn 'composition))

;; the dispatcher function
(define composition
  (case-lambda
    ; base case
    [(F G) (export 
            (cond 
              [(and (fixed-arity? F) (fixed-arity? G))
               ; some optimisation
               (cond 
                 [(and (unary? F) (unary? G))  (λ (x) (F (G x)))]
                 [(and (binary? F) (unary? G))  (λ (x y) (F (G x) y))]
                 [(and (unary? F) (binary? G))  (λ (x y) (F (G x y)))]
                 [(and (binary? F) (binary? G))  (λ (x y z) (F (G x y) z))]
                 [else  (make-composition F G)])]
              ; regular composition
              [(and (fixed-arity? F) (unary? F))  (procedure-reduce-arity
                                                   (compose1 F G)
                                                   (procedure-arity G))]
              ; general case
              [else  (make-composition F G)]))]
    ; many functions
    [(F G . H)  (export (apply make-composition F G H))]))

;; renaming
(define ∘ composition)

;; the stack-machene
(define make-composition
  (λ F (let ([R (reverse F)])
         (procedure-reduce-arity
          (cond 
            [(andmap (andf unary? fixed-arity?) (cdr R))
             (apply compose1 F)]
            [else  (λ A (apply values (feed-foldr A R)))])
          (apply nest-arity F)))))

;; the folding the stack
(define (feed-foldr x0 F)
  (cond
    [(one-element-list? F) (let ([f (car F)])
                             (cond
                               [(> (length x0) (normalized-min-arity f)) (list (apply f x0))]
                               [(or (variadic? f) (polyadic? f)) (list (apply f x0))]
                               [else (feed f x0)]))]
    [else (feed-foldr (feed (car F) x0) (cdr F))]))

;; the feeding the stack
(define (feed f stack)
  (define ar (normalized-min-arity f))
  (cond
    [(and (null? stack) (nullary? f)) (list (f))]
    [(greedy? f) (list (apply f stack))]
    [(nullary*? f) (cons (f (car stack)) (cdr stack))]
    [(< (length stack) ar)  (apply f stack)]
    [(= 0 ar) (cons (f) stack)]
    [(= 1 ar) (cons (f (car stack)) (cdr stack))]
    [else (let-values ([(head tail) (split-at stack (normalized-min-arity f))])
            (cons (apply f head) tail))]))

;;greedy variadic functions
(define greedy? 
  (flat-named-contract 'greedy? 
                       (λ (x) (check-tag 'greedy x))))

(define (greedy f) 
  ((set-tag 'greedy) 
   (procedure-reduce-arity f (maximal-arity f))))