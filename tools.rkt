#lang racket/base
;;______________________________________________________________
;;                   ______ 
;;                  (  //   _____ ____   .  __  __
;;                   ~//~ ((_)// // / / // ((_ ((_/_
;;                 (_//
;;..............................................................
;; Provides different handy tools.
;;;=============================================================
(require racket/contract
         racket/dict
         racket/match
         "tags.rkt"
         "arity.rkt"
         "functionals.rkt"
         "curry.rkt"
         "nest.rkt")

(provide
 (all-from-out "functionals.rkt"
               "curry.rkt")
 (except-out (all-from-out
              "arity.rkt"
              "nest.rkt")
             reduce-arity fixed-arity add-arity feed nest-arity)
 ; functional forms
 ==>
 (rename-out [or* or]
             [and* and])
 xor
 ; functions
 (contract-out
  (any-args (-> procedure? procedure?))
  (all-args (-> procedure? procedure?))
  (almost-equal? (-> any/c any/c boolean?))
  (tolerance (parameter/c real?))
  (symbol<? (->* (symbol? symbol?) #:rest (listof symbol?) boolean?))
  (pair<? (->* (pair? pair?) #:rest (listof pair?) boolean?))
  (ordered? (->* () #:rest list? boolean?))))

;;;=============================================================
;;; Logics
;;;=============================================================

;;;-------------------------------------------------------------
;; implication functional form
;;;-------------------------------------------------------------
(define-syntax ==>
  (syntax-id-rules ()
    [(==> a b) (if a b #t)]
    [==> (procedure-rename (λ (a b) (==> a b)) '==>)]))

;;;-------------------------------------------------------------
;; or functional form
;;;-------------------------------------------------------------
(define or**
  (case-lambda 
    [(x y) (or x y)]
    [(x y . z) (or x (apply or** y z))]))

(define-syntax or*
  (syntax-id-rules ()
    [(or* arg ...) (or arg ...)]
    [or* (procedure-rename or** 'or)]))

;;;-------------------------------------------------------------
;; and functional form
;;;-------------------------------------------------------------
(define and**
  (case-lambda 
    [(x y) (and x y)]
    [(x y . z) (and x (apply and** y z))]))

(define-syntax and*
  (syntax-id-rules ()
    [(and* arg ...) (and arg ...)]
    [and* (procedure-rename and** 'and)]))

;;;-------------------------------------------------------------
;; xor function
;;;-------------------------------------------------------------
(define (xor x y) (if x (not y) y))

;;;-------------------------------------------------------------
;;; any-args and all-args functionals
;;;-------------------------------------------------------------
(define (any-args p) (-< or* p))
(define (all-args p) (-< and* p))

;;;-------------------------------------------------------------
;; almost-equal? and tolerance
;;;-------------------------------------------------------------
(define tolerance (make-parameter 5e-16))

(define inexact-number? (andf number? inexact?))

(define (almost-equal? x y)
  (cond
    [((all-args real?) x y) (if ((any-args inexact-number?) x y)
                                (let ([ε (tolerance)])
                                  (or (and (< x ε) (< y ε))
                                      (< (abs (/ (- x y) (+ x y))) ε)))
                                (= x y))]
    
    [((any-args complex?) x y) ((andf (-< almost-equal? angle)
                                      (-< almost-equal? magnitude)) x y)]
    
    [((all-args pair?) x y) ((andf (-< almost-equal? car)
                                   (-< almost-equal? cdr)) x y)]
    
    [else (equal? x y)]))

;;;-------------------------------------------------------------
;; generic ordering function
;;;-------------------------------------------------------------
(define (ordered? . x)
  (apply 
   (case-lambda
     [() #t]
     [(x) #t]
     [(x y) (for/or ([px (in-list (dict-keys (type-ordering)))]
                     [i (in-naturals)]
                     #:when (px x))
              (for/and ([(py f) (in-dict (type-ordering))]
                        [j (in-range (+ i 1))]
                        #:when (py y))
                (and (= i j) (f x y))))]
     [(x y . z) (and (ordered? x y)
                     (apply ordered? y z))]) x))

;; ordering for booleans #t < #f
(define (boolean<? x y) (and x (not y)))

;; ordering for symbols
(define symbol<? 
  (-< string<? symbol->string))

;; ordering for pairs
(define pair<?
  (orf (-< ordered? car)
       (andf (-< equal? car)
             (-< ordered? cdr))))

;; type-ordering = [(<type> . <ordering-function>) ... ]
;; the oreder in the list defines the order of types
;; if compared objects have the same type 
;; the <ordering-function> is used to compare them.
(define type-ordering 
  (make-parameter (list (cons boolean? boolean<?)
                        (cons real? <)
                        (cons string? string<?)
                        (cons symbol? symbol<?)
                        (cons null? (const #f))
                        (cons pair? pair<?))))
