#lang formica
(define-type amb)

#;(:: real-sqrt (Real -> amb?)
    (define (real-sqrt x)
      (if (< x 0)
          (amb)
          (let ([y (sqrt x)]) 
            (amb y (- y))))))

#;(:: amb-plus (amb? amb? -> amb?)
  (define/. amb-plus
    (amb x ...) (amb y ...) --> (apply amb (append x y))))

(:: amb-plus (amb? amb? -> amb?)
    (define/. amb-plus
      (amb) x --> x
      (amb x _ ...) _ --> (amb x)))

(:: amb-bind (amb? (Any -> amb?) -> amb?)
  (define/. amb-bind
    (amb x ...) f --> (foldr (∘ amb-plus f) (amb) x)))

(define-syntax amb-do
  (syntax-rules (<-)
    [(amb-do (x <- A) res) (amb-bind A (λ (x) res))]
    [(amb-do A res) (amb-bind A (λ (_) res))]
    [(amb-do A B ... res) (amb-do A (amb-do B ... res))]))


(:: guard (Any -> amb?)
  (define (guard p)
    (if p (amb null) (amb))))

(:: real-sqrt (Real -> amb?)
  (define (real-sqrt x)
    (amb-do (guard (>= x 0))
            (y <- (amb (sqrt x)))
            (amb y (- y)))))



