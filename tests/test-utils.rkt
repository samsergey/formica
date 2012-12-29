#lang racket/base
;; Formica lang
;;
;; test-utils module
;; 
;; Provides some usefull testing utilities

(require rackunit
         racket/list
         racket/syntax)


(provide n u b t n* u* b* v v* o 
         test-functions
         check-predicate
         check-axiomatic
         sufficient-argument-list
         (all-from-out rackunit)
         track-lines-of-interest)

;; Formal functions with different arities.
;; a nullary function
(define n (λ () (list 'n)))
;; an unary function
(define u (λ (x) (list 'u x)))
;; a binary function
(define b (λ (x y) (list 'b x y)))
;; a ternary function
(define t (λ (x y z) (list 't x y z)))
;; variadic functions
(define n* (λ x (list* 'n* x)))
(define u* (λ (x . z) (list* 'u* x z)))
(define b* (λ (x y . z) (list* 'b* x y z)))
;; polyadic function
(define v (case-lambda [(x) (list 'v x)]
                       [(x y) (list 'v x y)]
                       [(x y z t) (list 'v x y z t)]))
;; polyadic and variadic
(define v* (case-lambda [(x) (list 'v* x)]
                        [(x y) (list 'v* x y)]
                        [(x y z . t) (list* 'v* x y z t)]))
;; polyadic with optional arguments
(define (o x (y 88)) (list 'o x y))
;; list of all test-functions
(define test-functions
  (list n u b t n* u* b* v v* o))

;; Form which generates predicate tests for positive and negative sets
(define-syntax-rule (check-predicate pred (positive ...) (negative ...))
  (begin
    (check-true (pred positive)) ...
    (check-false (pred negative)) ...))

;; Form which generates tests in order to check that for all elements of the test-set
;; if the precondition is satisfied then function's result satisfies the postcondition
;; and the exeption is raized otherwize.
(define-syntax check-axiomatic
  (syntax-rules ()
    [(_ (sets ...) pre (r fun) post)
     (for* (sets ...)
       (let* ([preconditions pre]
              [r (if preconditions 
                     fun
                     (check-exn exn:fail? (λ () fun)))]
              [postconditions (or (void? r) post)])
         (check-true postconditions)))]))

(define (sufficient-argument-list f)
  (cond
    [(procedure? f) (sufficient-argument-list (procedure-arity f))]
    [(list? f) (append-map sufficient-argument-list f)]
    [(arity-at-least? f) (sufficient-argument-list (list (arity-at-least-value f)
                                                         (+ 1 (arity-at-least-value f))))]
    [(integer? f) (list (range f))]))

(define-syntax-rule (track-lines-of-interest)
  (module loi racket/base
    (require racket/set 
             (for-syntax racket/base))
    (define candidate-lines (set))
    (define touched-lines (set))
    (define (visited line)
      (unless (set-member? touched-lines line)
        (set! touched-lines (set-add touched-lines line))
        (displayln
         (sort (set->list
                (set-subtract candidate-lines touched-lines))
               <))))
    (provide line-of-interest)
    (define-syntax (line-of-interest stx)
      (with-syntax ([line (syntax-line stx)])
        (syntax-local-lift-expression
         #'(set! candidate-lines (set-add candidate-lines line)))
        #'(visited line)))))