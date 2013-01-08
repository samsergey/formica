#lang racket/base
(require "../private/tools/nest.rkt"
         "automatic-nest-tests.rkt"
         "test-utils.rkt"
         rackunit)

(test-case
 "add-arity tests"
 (check-equal? (add-arity 2 2) 3)
 (check-equal? (add-arity 1 2) 2)
 (check-equal? (add-arity 2 1) 2)
 (check-equal? (add-arity 2 0) 1)
 (check-equal? (add-arity 1 0) 0)
 (check-equal? (add-arity 0 2) #f)
 (check-equal? (add-arity 2 '(1 2)) 2)
 (check-equal? (add-arity '(1 4) 2) '(2 5))
 (check-equal? (add-arity '(1 4) '(1 2)) '(1 4))
 (check-equal? (add-arity '(0 2) 1) 2)
 (check-equal? (add-arity '(0 2) '(0 2)) (add-arity '(0 2) 0))
 (check-equal? (add-arity 2 (arity-at-least 2)) 3)
 (check-equal? (add-arity 2 (arity-at-least 0)) 1)
 (check-equal? (add-arity 1 (arity-at-least 0)) 0)
 (check-equal? (add-arity 2 (arity-at-least 2)) 3)
 (check-equal? (add-arity (arity-at-least 0) 1) (arity-at-least 1))
 (check-equal? (add-arity (arity-at-least 2) 1) (arity-at-least 2))
 (check-equal? (add-arity (arity-at-least 2) (arity-at-least 2)) (arity-at-least 3))
 (check-equal? (add-arity 2 `(1 ,(arity-at-least 2))) 2)
 (check-equal? (add-arity 2 `(2 ,(arity-at-least 0))) (add-arity 2 (arity-at-least 0)))
 (check-equal? (add-arity (arity-at-least 2) `(1 ,(arity-at-least 2))) (arity-at-least 2)))


(test-case
 "feeding"
 (check-equal? (feed n '(1 2 3)) '((n) 1 2 3))
 (check-equal? (feed u '(1 2 3)) '((u 1) 2 3))
 (check-equal? (feed b '(1 2 3)) '((b 1 2) 3))
 (check-equal? (feed n* '(1 2 3)) '((n* 1) 2 3))
 (check-equal? (feed u* '(1 2 3)) '((u* 1) 2 3))
 (check-equal? (feed b* '(1 2 3)) '((b* 1 2) 3))
 (check-equal? (feed n '()) '((n)))
 (check-equal? (feed n* '()) '((n*)))
 (check-exn exn:fail:contract:arity? (λ () (feed b '(1))))
 (check-exn exn:fail:contract:arity? (λ () (feed b '(1))))
 (check-equal? (feed u* '(1 2 3)) '((u* 1) 2 3))
 (check-equal? (feed (greedy u*) '(1 2 3)) '((u* 1 2 3))))

(two-functions)
;(three-functions)
(right-identity)
(left-identity)
(associativity)

(check-equal? ((∘ u (greedy n*)) 1 2 3) (u (n* 1 2 3)))