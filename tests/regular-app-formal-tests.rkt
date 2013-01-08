#lang racket/base
(require rackunit
         "../private/regular-app/formal.rkt"
         racket/list
         racket/match)

(define-formal f (g 2) (h '(1 3)) (F (arity-at-least 2)))

(test-case 
 "formal-function? testing"
 (check-true (andmap formal-function? (list f g h F)))
 (check-true (formal-function? (hold + 2)))
 (check-true (formal-function? (hold 'f)))
 (check-false (formal-function? +))
 (check-false (formal-function? '+))
 (check-false (formal-function? 8)))

(test-case
 "formal? predicate"
 
 (check-false (formal? '(+ 1 2)) "The formal result of a function which was not declared to be formal")
 (check-true (formal? (f 1 2)) "The formal result of a function which was declared to be formal")
 (check-false (formal? (($ f) 1 2)) "The formal result of a function which was not declared to be formal")
 (check-false (formal? 8)))

(test-case
 "formal functions"
 
 (check-equal? (f)  '(f))
 (check-equal? (f 'a)  '(f a))
 (check-equal? (f 'a 1)  '(f a 1))
 
 (check-equal? (procedure-arity g) 2) 
 (check-equal? (procedure-arity F)  (arity-at-least 2))
 
 (check-exn exn:fail:contract:arity? (λ () (g 1)))
 (check-exn exn:fail:contract:arity? (λ () (F 1)))
 (check-exn exn:fail:contract:arity? (λ () (h 1 2))))

(test-case
 "formal application"
 (check-true (f? (f 'a 1)))
 (check-true (f? '(f a 1)))
 (check-false (g? (f 'a 1)))
 (check-false (g? '(g 1)))
 (check-false (h? '(h 1 2)))
 (check-false (F? '(F 2)))
 (check-true (g? (g 'a 1)))
 (check-false (f? 5)))

(test-case
 "non-formal lists and pairs"
 (check-true (n/f-list? '(+ a 1)))
 (check-false (n/f-list? (f 'a 1)))
 (check-true (n/f-list? (list '+ 'a)))
 (check-true (n/f-list? '()))
 
 (check-true (n/f-pair? '(a b)))
 (check-false (n/f-pair? (f 'a)))
 (check-true (n/f-pair? (cons '+ 'a)))
 (check-false (n/f-pair? '())))

(test-case
 "matching formal applications"
 (check-equal? (match (f 2) [(f a) a])  2)
 (check-equal? (match (f 1 2 3) [(f a ...) a])  '(1 2 3))
 (check-equal? (match (g 1 2) [(g a b) (list a b)])  '(1 2))
 (check-equal? (match (g 1 2) [(g a ...) a])  '(1 2))
 (check-equal? (match (h 1) [(h a) a]) 1)
 (check-equal? (match (h 1 2 3) 
                 [(h a b c) c])  
               3)
 (check-equal? (match (h 1 2 3) 
                 [(h a ...) a])  
               '(1 2 3))
 (check-equal? (match (h (f 1) (g 2 3) 4) 
                 [(h (f a) (g b c) d) (list a b c d)])  '(1 2 3 4))
 
 (check-equal? (match (h 1) 
                 [(formal (x a)) `(fun: ,x arg: ,a)]) 
               '(fun: h arg: 1)))

(require (prefix-in ~ (only-in "test-utils.rkt" n u b t n* u* b* v v*)))

(test-case
 "Equivalence of formal functions and test functions"
 
 (define-formal 
   (n 0) (u 1) (b 2) (t 3) n* 
   (u* (arity-at-least 1))
   (b* (arity-at-least 2))
   (v '(1 2 4))
   (v* `(1 2 ,(arity-at-least 4))))
 
 (check-equal? (n) (~n))
 (check-equal? (u 1) (~u 1))
 (check-equal? (b 1 2) (~b 1 2))
 (check-equal? (t 1 2 3) (~t 1 2 3))
 (check-equal? (n*) (~n*))
 (check-equal? (n* 1 2 3) (~n* 1 2 3))
 (check-equal? (u* 1) (~u* 1))
 (check-equal? (u* 1 2 3) (~u* 1 2 3))
 (check-equal? (b* 1 2) (~b* 1 2))
 (check-equal? (b* 1 2 3) (~b* 1 2 3))
 (check-equal? (v 1) (~v 1))
 (check-equal? (v 1 2) (~v 1 2))
 (check-equal? (v 1 2 3 4) (~v 1 2 3 4))
 (check-equal? (v* 1) (~v* 1))
 (check-equal? (v* 1 2) (~v* 1 2))
 (check-equal? (v* 1 2 3 4 5) (~v* 1 2 3 4 5)))