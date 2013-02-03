#lang formica
(define-type amb)

(:: n-sqrt (Real -> (∪ (amb:) (amb: Real Real)))
  (define (n-sqrt x)
    (if (and (real? x) (positive? x))
        (let ([y (sqrt x)]) 
          (amb (- y) y))
        (amb))))

(:: amb-plus (amb? amb? -> amb?)
  (define/. amb-plus
    (amb x ...) (amb y ...) --> (apply amb (append x y))))

(:: amb-bind (amb? (Any -> amb?) -> amb?)
  (define/. amb-bind
    (amb x ...) f --> (foldr (∘ amb-plus f) (amb) x)))

(define-syntax amb-do
  (syntax-rules (<-)
    [(amb-do (x <- A) res) (amb-bind A (λ (x) res))]
    [(amb-do A B ... res) (amb-do A (amb-do B ... res))]))

(:: assert (Any -> amb?)
  (define (assert p)
    (if p (amb null) (amb))))


