#lang racket/base
;;______________________________________________________________
;;                   ______ 
;;                  (  //   _____ ____   .  __  __
;;                   ~//~ ((_)// // / / // ((_ ((_/_
;;                 (_//
;;..............................................................
;; Provides generic ordering function.
;;;=============================================================
(require racket/dict
         "types.rkt"
         "../tools/functionals.rkt")

(provide
 ordered?
 (contract-out
  (type-ordering (parameter/c (list: (cons: contract? Fun) ..)))
  (add-to-type-ordering (->* (contract?) (contract? Fun) void?))
  (symbol<? (->* (Sym Sym) #:rest (list: Sym ..) Bool))
  (pair<? (->* (pair? pair?) #:rest (list: pair? ..) Bool))))


;; generic ordering function.
(define (ordered? . x)
  (apply 
   (case-lambda
     [() #t]
     [(x) #t]
     [(x y) (for/or ([px (in-list (dict-keys (type-ordering)))]
                     [i (in-naturals)]
                     #:when (is x px))
              (for/and ([(py f) (in-dict (type-ordering))]
                        [j (in-range (+ i 1))]
                        #:when (is y py))
                (and (= i j) (f x y))))]
     [(x y . z) (and (ordered? x y)
                     (apply ordered? y z))]) x))

;; ordering for booleans #t < #f
(define (boolean<? x y) (and x (not y)))

;; ordering for symbols
(define symbol<? 
  (/@ string<? symbol->string))

;; ordering for pairs
(define pair<?
  (orf (/@ ordered? car)
       (andf (/@ equal? car)
             (/@ ordered? cdr))))

(define (add-to-type-ordering type (prec-type 'last) (ord-fun (const #f)))
  (type-ordering 
   (let ([new-type (cons type ord-fun)])
     (cond
       [(eq? prec-type 'last) (append (type-ordering) (list new-type))]
       [(eq? prec-type 'first) (append (list new-type) (type-ordering))]
       [(dict-has-key? (type-ordering) type) (type-ordering (dict-remove (type-ordering) type))
                                             (add-to-type-ordering type prec-type ord-fun)
                                             (type-ordering)]
       [else (let next ([lst (type-ordering)])
               (cond
                 [(null? lst) (list new-type)]
                 [(equal? (caar lst) prec-type) (list* (car lst) 
                                                       new-type
                                                       (cdr lst))]
                 [else (cons (car lst) (next (cdr lst)))]))]))))

;; type-ordering = [(<type> . <ordering-function>) ... ]
;; the oreder in the list defines the order of types
;; if compared objects have the same type 
;; the <ordering-function> is used to compare them.
(define type-ordering 
  (make-parameter (list (cons #t (const #f))
                        (cons #f (const #f))
                        (cons real? <)
                        (cons string? string<?)
                        (cons symbol? symbol<?)
                        (cons null? (const #f))
                        (cons pair? pair<?))))

