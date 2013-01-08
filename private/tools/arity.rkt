#lang racket/base
;;______________________________________________________________
;;                   ______ 
;;                  (  //   _____ ____   .  __  __
;;                   ~//~ ((_)// // / / // ((_ ((_/_
;;                 (_//
;;..............................................................
;; Provides tools to work with function arity.
;;==============================================================

(require (only-in racket/list argmin argmax)
         racket/contract)

(provide 
 (contract-out
  (polyadic? predicate/c)
  (variadic? predicate/c)
  (fixed-arity? predicate/c)
  (nullary? predicate/c)
  (unary? predicate/c)
  (binary? predicate/c)
  (min-arity (-> procedure? (or/c 0 positive?)))
  (max-arity (-> procedure? (or/c 0 positive? +inf.0)))
  (fixed-arity (-> procedure-arity? procedure-arity?))
  (reduce-arity (-> procedure-arity? integer? procedure-arity?))
  (inherit-arity (-> procedure? (-> procedure? procedure?)))))

;; Predicate. Specifies polyadic functions.
(define (polyadic? f) 
  (and (procedure? f) 
       (list? (procedure-arity f))))

;; Predicate. Specifies variadic functions.
(define (variadic? f) 
  (and (procedure? f) 
       (let ([ar (procedure-arity f)])
         (if (list? ar) 
             (ormap arity-at-least? ar)
             (arity-at-least? ar)))))

;; Predicate. Specifies variadic functions.
(define (fixed-arity? f) 
  (and (procedure? f) 
       (integer? (procedure-arity f))))

;; Predicate. Specifies variadic nullary functions.
(define (nullary? f) 
  (and (procedure? f)
       (procedure-arity-includes? f 0)))

;; Predicate. Specifies variadic unary functions.
(define (unary? f) 
  (and (procedure? f)
       (procedure-arity-includes? f 1)))

;; Predicate. Specifies variadic binary functions.
(define (binary? f) 
  (and (procedure? f)
       (procedure-arity-includes? f 2)))

;; Returns minimal acceptible arity for a function
(define (min-arity f) 
  (let result ([ar (procedure-arity f)])
    (cond
      [(list? ar) (argmin result ar)]
      [(null? ar) 0]
      [(arity-at-least? ar) (arity-at-least-value ar)]
      [else ar])))

;; Returns maximal acceptible arity for a function
(define (max-arity f)
  (let result ([ar (procedure-arity f)])
    (cond
      [(null? ar) 0]
      [(list? ar) (apply max (map result ar))]
      [(arity-at-least? ar) +inf.0]
      [else ar])))

;; Fixes variable arity
(define (fixed-arity ar)
  (cond
    [(arity-at-least? ar) (arity-at-least-value ar)]
    [(list? ar) (map fixed-arity ar)]
    [else ar]))


;; Arity reduction by an integer
(define (reduce-arity a n)
  (cond
    [(integer? a) (if (<= n a) (- a n) (error 'reduce-arity 
                                              (format "Cannot reduce arity ~a by ~a." a n)))]
    [(list? a) (let ([possible (filter (λ (x) (or (and (integer? x) (>= x n)) (arity-at-least? x))) a)])
                 (when (null? possible) (error 'reduce-arity 
                                               (format "Cannot reduce arity ~a by ~a." a n)))
                 (map (λ (x) (reduce-arity x n)) possible))]
    [(arity-at-least? a) (arity-at-least (max 0 (- (arity-at-least-value a) n)))]))

(define ((inherit-arity f) g)
  (procedure-reduce-arity g (procedure-arity f)))