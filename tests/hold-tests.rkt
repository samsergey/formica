#lang racket/base
(require "test-utils.rkt"
         "../private/formal/hold.rkt")

(test-case 
 "hold testing"
 
 (check-equal? (procedure-arity (hold 'f))  (arity-at-least 0))
 (check-equal? (procedure-arity (hold +))  (procedure-arity +))
 (check-equal? (procedure-arity (hold cons))  (procedure-arity cons))
 (check-equal? (procedure-arity (hold cons 3))  3)
 
 (check-equal? ((hold 'f))  '(f))
 (check-equal? ((hold 'f) 1)  '(f 1))
 (check-equal? ((hold 'f) 1 2)  '(f 1 2))
 (check-equal? ((hold 'f 2) 1 2)  '(f 1 2))
 (check-exn exn:fail:contract:arity? (λ () ((hold 'f 2) 1)))
 (check-exn exn:fail:contract:arity? (λ () ((hold 'f 2) 1 2 3)))
 
 (check-equal? ((hold +))  '(+))
 (check-equal? ((hold +) 1)  '(+ 1))
 (check-equal? ((hold +) 1 2)  '(+ 1 2))
 (check-equal? ((hold + 2) 1 2)  '(+ 1 2))
 
 (check-equal? (foldr ($ +) 0 '(1 2 3))  '(+ 1 (+ 2 (+ 3 0)))))

(test-case 
 "formal-function? testing"
 (check-true (formal-function? (hold +)))
 (check-true (formal-function? (hold + 2)))
 (check-true (formal-function? (hold 'f)))
 (check-false (formal-function? +))
 (check-false (formal-function? '+))
 (check-false (formal-function? 8)))
