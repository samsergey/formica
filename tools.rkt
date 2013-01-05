#lang racket
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
             [and* and]
             [eq?* eq?]
             [equal?* equal?]
             [almost-equal?* almost-equal?])
 ; functions
 different?
 xor ≈
 ordered/c
 (contract-out
  (any-args (-> procedure? procedure?))
  (all-args (-> procedure? procedure?))
  (tolerance (parameter/c real?))
  (ordered? (->* () #:rest (listof ordered/c) boolean?))  
  (symbol<? (->* (symbol? symbol?) #:rest (listof symbol?) boolean?))
  (pair<? (->* (pair? pair?) #:rest (listof pair?) boolean?))))

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
    [((all-args real?) x y) 
      (cond 
       [((any-args inexact-number?) x y) (or (= x y)
                                              (< (+ x y) (tolerance))
                                              (< (abs (/ (- x y) (+ x y))) (tolerance)))]
       [else (= x y)])]
    
    [((andf (all-args number?)
            (any-args complex?)) x y) (almost-equal? 0 (magnitude (- x y)))]
    
    
    [((all-args pair?) x y) ((andf (-< almost-equal? car)
                                   (-< almost-equal? cdr)) x y)]
    
    [else (equal? x y)]))

(define eq?* 
  (case-lambda 
    [(x y) (eq? x y)]
    [(x y . z) (and (eq? x y) (apply eq?* y z))]))

(define equal?* 
  (case-lambda 
    [(x y) (equal? x y)]
    [(x y . z) (and (equal? x y) (apply equal?* y z))]))

(define almost-equal?* 
  (case-lambda 
    [(x y) (almost-equal? x y)]
    [(x y . z) (and (almost-equal? x y) (apply almost-equal?* y z))]))

(define ≈ almost-equal?*)

(define different?
  (case-lambda 
    [(x y) (not (equal? x y))]
    [(x y . z) (or (not (equal? x y)) 
                   (ormap (λ (t) (different? x t)) z)
                   (apply different? y z))]))
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

(define ordered/c
  (flat-rec-contract ordered/c 
                     (or/c boolean?
                           real?
                           string?
                           symbol?
                           null?
                           (cons/c ordered/c ordered/c))))
