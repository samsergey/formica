#lang racket/base
(require "test-utils.rkt"
         "../partial-app.rkt"
         (except-in "../tools.rkt" +)
         "../formal.rkt"
         racket/list)

(test-case 
 "simple partial application tests"
 (check-true (curried? (add1)))
 (check-equal? (add1 1) 2)
 
 (check-true (curried? (cons)))
 (check-true (curried? (cons 1)))
 (check-equal? (cons 1 2) '(1 . 2))
 (check-equal? ((cons) 1 2) '(1 . 2))
 (check-equal? (((cons) 1) 2) '(1 . 2))
 (check-equal? ((cons 1) 2) (cons 1 2))
 
 (check-equal? ((((curry) +) 1) 2) 3))

(test-case 
 "partial composition tests"
 (check-equal? (((∘ cons cons) 1) 2 3) '((1 . 2) . 3))
 (check-equal? (((∘ cons cons) 1 2) 3) '((1 . 2) . 3))
 (check-equal? ((((∘ cons cons)) 1 2) 3) '((1 . 2) . 3)))

(for* ([f test-functions]
       [a (in-value (min-arity f))]
       [s a])
  (let ([x (build-list a values)])
    (let-values ([(l r) (split-at x s)])
      (check-equal? (apply (apply f l) r) (apply f x)))))

(test-case
 "apply function"
 (check-equal? (apply cons '(1 2))  (cons 1 2))
 (check-equal? (apply + '(1 2))  3)
 (check-equal? (apply + 1 '(2 3))  6)
 (check-equal? (apply - 1 '(2 3))  -4)
 (check-equal? (apply - '(2 3))  -1)
 (check-equal? (apply - '(3))  -3)
 (check-equal? (apply apply (list - '(3)))  -3)
 (check-equal? (apply list 1 2 '(3))  '(1 2 3))
 (check-equal? ((apply apply) (list - '(3)))  -3)
 (check-equal? ((apply +) '(1 2 3)) 6)
 (check-equal? ((apply) cons '(1 2)) '(1 . 2)))

(define-formal (g 2) (h '(1 3)))

(test-case
 "formal functions"
 (check-true (curried? ((hold 'f 2) 1)))
 (check-true (curried? ((hold cons) 1)))
 (check-equal? (((hold 'f 2) 1) 2)  '(f 1 2))
 (check-equal? (((hold cons) 1) 2)  '(cons 1 2))
 (check-true (curried? (b 1)))
; (check-equal? ((g 1) 2)  '(g 1 2))
; (check-true (curried? (h)))
; (check-true (curried? (h 1 2)))
; (check-equal? ((h) 1)  '(h 1))
; (check-equal? (h 1) '(h 1))
; (check-equal? ((h 1 2) 3)  '(h 1 2 3))
; (check-equal? (map (g 1) '(x y z)) 
;               '((g 1 x) (g 1 y) (g 1 z)))
 )





