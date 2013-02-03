#lang formica/regular-app
(define-type Amb)

(:: amb-plus (Amb? Amb? -> Amb?)
  (define/. amb-plus
    (Amb x ...) (Amb y ...) --> (apply return (append x y))))

(:: amb-bind (Amb? (Any -> Amb?) -> Amb?)
  (define/. amb-bind
    (Amb x ...) f --> (foldr (∘ amb-plus f) (Amb) x)))

(define-monad Amb-monad
  #:type Amb
  #:return (λ x (apply Amb (remove-duplicates x)))
  #:bind amb-bind
  #:mplus amb-plus
  #:mzero (Amb))

(define-type (Just Any))
(define-type Maybe?
  Just?
  'Nothing)

(define-monad Maybe
  #:type Maybe?
  #:return (λ (x . y) (Just x))
  #:bind (/. (Just x) f --> (f x)
             'Nothing f --> 'Nothing)
  #:mplus (/. 'Nothing x --> x
              x 'Nothing --> x
              x y --> x)
  #:mzero 'Nothing)

(define (real-sqrt x)
  (do (guard (>= x 0))
      (y <- (return (sqrt x)))
      (return y (- y))))

(using Maybe (real-sqrt 4))

