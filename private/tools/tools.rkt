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
         "functionals.rkt")

(provide
 ; functional forms
 ==>
 (rename-out [or* or]
             [and* and]
             [eq?* eq?]
             [equal?* equal?]
             [almost-equal?* almost-equal?])
 ; functions
 different?
 xor ≈ ≥ ≤
 (contract-out
  (any-args (-> procedure? procedure?))
  (all-args (-> procedure? procedure?))
  (tolerance (parameter/c real?))))

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
(define (any-args p) (/@ or* p))
(define (all-args p) (/@ and* p))

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
    
    
    [((all-args pair?) x y) ((andf (/@ almost-equal? car)
                                   (/@ almost-equal? cdr)) x y)]
    
    [else (equal? x y)]))

;;;-------------------------------------------------------------
;; variadic eq?
;;;-------------------------------------------------------------
(define eq?* 
  (case-lambda 
    [(x y) (eq? x y)]
    [(x y . z) (and (eq? x y) (apply eq?* y z))]))

;;;-------------------------------------------------------------
;; variadic equal?
;;;-------------------------------------------------------------
(define equal?* 
  (case-lambda 
    [(x y) (equal? x y)]
    [(x y . z) (and (equal? x y) (apply equal?* y z))]))

;;;-------------------------------------------------------------
;; variadic almost-equal?
;;;-------------------------------------------------------------
(define almost-equal?* 
  (case-lambda 
    [(x y) (almost-equal? x y)]
    [(x y . z) (and (almost-equal? x y) (apply almost-equal?* y z))]))

(define ≈ almost-equal?*)

;;;-------------------------------------------------------------
;; different? predicate
;;;-------------------------------------------------------------
(define different?
  (case-lambda 
    [(x y) (not (equal? x y))]
    [(x y . z) (and (not (equal? x y)) 
                    (andmap (λ (t) (different? x t)) z)
                    (apply different? y z))]))

(define ≥ >=)
(define ≤ <=)

