#lang racket/base
;;______________________________________________________________
;;                   ______ 
;;                  (  //   _____ ____   .  __  __
;;                   ~//~ ((_)// // / / // ((_ ((_/_
;;                 (_//
;;..............................................................
; Provides different functionals and operators.
;;;=============================================================
(require racket/contract
         racket/match
         "tags.rkt"
         "arity.rkt")

(provide
 id 
 I1 I2 I3
 const
 (contract-out
  (negated (-> procedure? procedure?))
  (¬ (-> procedure? procedure?))
  (flipped (-> procedure? procedure?))
  (arg (-> natural-number/c procedure?))
  (fif (-> procedure? procedure? procedure? procedure?))
  (andf (->* () #:rest (listof procedure?) procedure?))
  (orf (->* () #:rest (listof procedure?) procedure?))
  (argmap (-> procedure? unary? procedure?))
  (/@ (-> procedure? unary? procedure?))
  (fixed-point (->* (procedure?) (#:same-test (-> any/c any/c boolean?)) procedure?))))

;;;-------------------------------------------------------------
;;; id
;;;-------------------------------------------------------------
(define (id x) x)

;;;-------------------------------------------------------------
;;; arg
;;;-------------------------------------------------------------
(define (arg n)
  (define result (λ x (list-ref x (- n 1))))
  ((set-tag* 'arg (string->symbol (format "~a" n)))
   (procedure-reduce-arity result (arity-at-least n))))


(define I1 (arg 1))
(define I2 (arg 2))
(define I3 (arg 3))

;;;-------------------------------------------------------------
;;; const
;;;-------------------------------------------------------------
(define (const x)
  (define result (λ y x))
  (procedure-rename result 'const))

;;;-------------------------------------------------------------
;;; negate
;;;-------------------------------------------------------------
(define (negated f)
  (define result (compose1 not f))
  ((set-tag* 'negated (or (object-name f) 'λ))
   ((inherit-arity f) 
    result)))

(define ¬ negated)

;;;-------------------------------------------------------------
;;; flip
;;;-------------------------------------------------------------
(define (flipped f)
  (define result (λ x (apply f (reverse x))))
  ((set-tag* 'flipped (or (object-name f) 'λ))
   ((inherit-arity f) 
    result)))

;;;-------------------------------------------------------------
;;; fif
;;;-------------------------------------------------------------
(define (fif p f g)
  (define result 
    (case-lambda
      [() (if (p) (f) (g))]
      [(x) (if (p x) (f x) (g x))]
      [(x y) (if (p x y) (f x y) (g x y))]
      [(x y z) (if (p x y z) (f x y z) (g x y z))]
      [(x y z . t) (if (apply p x y z t) 
                       (apply f x y z t) 
                       (apply g x y z t))]))
  (procedure-rename
   ((inherit-arity p) result)
   'fif))

;;;-------------------------------------------------------------
;;; andf
;;;-------------------------------------------------------------
(define andf
  (case-lambda
    [() (const #t)]
    [(f) f]
    [(f g) (procedure-rename (fif f g f) 'andf)]
    [(f . g) (andf f (apply andf g))]))

;;;-------------------------------------------------------------
;;; orf
;;;-------------------------------------------------------------
(define orf
  (case-lambda
    [() (const #f)]
    [(f) f]
    [(f g) (procedure-rename (fif f f g) 'orf)]
    [(f . g) (orf f (apply orf g))]))

;;;-------------------------------------------------------------
;;; argmap
;;;-------------------------------------------------------------
(define (argmap f g)
  (define result (λ x (apply f (map g x))))
   (procedure-rename
   ((inherit-arity f) result)
   'argmap))

(define /@ argmap)

;;;-------------------------------------------------------------
;;; fixed-point
;;;-------------------------------------------------------------
;: The iterative procedure which finds the fixed point of the function f
;: (any/c ... -> (values any/c ...)) -> (any/c ... -> (values any/c ...))
(define (fixed-point f #:same-test [eq-test equal?]) 
  ((set-tag* 'fixed-point (or (object-name f) 'λ))
   (if (and (fixed-arity? f) (unary? f))
       (λ (x)
         (let F ([x x] [fx (f x)])
           (if (eq-test x fx)
               fx
               (F fx (f fx)))))
       (procedure-reduce-arity
        (λ x 
          (let F ([x x] [fx (apply (compose list f) x)])
            (if (eq-test x fx)
                (apply values fx)
                (F fx (apply (compose list f) fx)))))
        (procedure-arity f)))))