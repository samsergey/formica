#lang formica
(provide (formal-out Just)
         Maybe)
(define-formal Just)

(define-type (Maybe? a) (Just: a) 'Nothing)

(define (Maybe a)
  (monad
   #:type (Maybe? a)
   #:return (/. 'Nothing --> 'Nothing
                x       --> (Just x))
   #:bind (/. 'Nothing f --> 'Nothing
              (Just x) f --> (f x))
   #:mzero 'Nothing
   #:mplus (/. 'Nothing x --> x
               x       _ --> x)))

(using-monad (Maybe Int))

(module+ test
(using (Maybe (list: Int Int))
  (do [(list x y) <- (Just '(1 2))]
      (return (list (+ x y) (- x y)))))

(define (isqrt x)
  (let next ([s 0] [r 0])
    (cond
      [(> s x) mzero]
      [(almost-equal? s x) (mplus (return r)
                                  (return (- r)))]
      [else (next (+ s (* 2 r) 1) (+ 1 r))])))

(using List
  (bind (return 4) >>= isqrt))

(using (Maybe Int)
  (bind (return 4) >>= isqrt)))