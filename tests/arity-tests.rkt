#lang racket/base
(require "../private/tools/arity.rkt"
         "test-utils.rkt")

(define-syntax-rule (==> a b) (if a b #t))

(test-case
 "exact arity tests"
 (check-predicate variadic?     (n* u* b* v*) (n u b t v o))
 (check-predicate polyadic?     (v v* o) (n u b t n* u* b*))
 (check-predicate fixed-arity?  (n u b t) (n* u* b* v o v*))
 (check-predicate nullary?      (n n*) (u b t u* b* v v* o))
 (check-predicate unary?        (u n* u* v v* o) (n b t b*))
 (check-predicate binary?       (n* u* b b* v v* o) (n u t)))

(test-case
 "min arity tests"
 (check-equal? (min-arity n) 0)
 (check-equal? (min-arity n*) 0)
 (check-equal? (min-arity u) 1)
 (check-equal? (min-arity u*) 1)
 (check-equal? (min-arity b) 2)
 (check-equal? (min-arity b*) 2)
 (check-equal? (min-arity t) 3)
 (check-equal? (min-arity v) 1)
 (check-equal? (min-arity v*) 1)
 (check-equal? (min-arity o) 1))

(test-case
 "max arity tests"
 (check-equal? (max-arity n) 0)
 (check-equal? (max-arity n*) +inf.0)
 (check-equal? (max-arity u) 1)
 (check-equal? (max-arity u*) +inf.0)
 (check-equal? (max-arity b) 2)
 (check-equal? (max-arity b*) +inf.0)
 (check-equal? (max-arity t) 3)
 (check-equal? (max-arity v) 4)
 (check-equal? (max-arity v*) +inf.0)
 (check-equal? (max-arity o) 2))

(test-case
 "reduce-arity tests"
 (let* ([ar '(0 1 2 3)]
        [ar* (map arity-at-least ar)]
        [arv (for*/list ([a ar] [b ar] #:when (< a b)) (list a b))]
        [arv* (for*/list ([a arv] [b ar*] #:when (< (apply max a) (fixed-arity b))) (append a (list b)))]
        [arities (append ar ar* arv arv*)])
   (check-axiomatic
    ; test set
    ([a arities] [b 3])
    
    ; preconditions
    (and (==> (integer? a) (<= b a))
         (==> (list? a) (<= b (apply max (map fixed-arity a)))))
    
    ; result
    (res (reduce-arity a b))
    
    ; postconditions
    (cond
      [(integer? a) (= res (- a b))]
      [(arity-at-least? a) (= (fixed-arity res) (max 0 (- (fixed-arity a) b)))]
      [(list? a) (equal? res (map (λ (x) (reduce-arity x b)) 
                                  (filter (λ (x) (or (and (integer? x) (>= x b)) (arity-at-least? x))) a)))]))))