#lang formica/regular-app
(define-formal (delayed 1))
(define-syntax-rule (~ expr)
  (delayed (λ () expr)))
(define//. !
  (delayed expr) --> (expr))

(define-syntax-rule (cons~ h t)
  (cons h (~ t)))

(:: car~ (pair? -> Any)
  (define (car~ s) (car s)))

(:: cdr~ (pair? -> Any)
  (define (cdr~ s) (! (cdr s))))

(:: foldr~ ((Any Any -> Any) Any list? -> Any)
  (define/c (foldr~ f x0 )
    (/. '() --> x0
        (cons h t) --> (! (f h (~ (foldr~ f x0 (! t))))))))

(define/c (map~ f) (foldr~ (∘ cons f) '()))

(define-type (Stream A)
  '()
  (cons: A delayed?))

(:: show ((Stream Any) (? Nat) -> list?)
  (define show
    (/. str   --> (show str 6)
        str 0 --> '()
        '() n --> '()
        (cons h t) n --> (cons h (show (! t) (- n 1))))))

(define (show-all s) (show s +inf.0))

(define (stream . x) (foldr~ cons '() x))

(define (cycle . x) (foldr~ cons (~ (apply cycle x)) x))

(define (aryth a s) (cons~ a (aryth (+ a s) s)))

(define (iterations f x) (cons~ x (iterations f (f x))))

(define (powers x) (iterations (* x) 1))

(define/c (until p) (foldr~ (λ (h t) (if (p h) '() (cons h t))) '()))
(define/c (skip-while p) (foldr~ (λ (h t) (if (p h) t (cons h t))) '()))

(define nats (aryth 0 1))

(define (euler h)
  (λ (f)
    (/. (list x y) --> (list (+ x h) 
                             (+ y (* h (f x y)))))))

(define (runge-kutta h)
  (λ (f)
    (/. (list x y) --> (list (+ x h) 
                             (+ y (* h (f (+ x (* h 1/2)) 
                                          (+ y (* h 1/2 (f x y))))))))))

(define (select-by p s)
  (until (negated p)
         (skip-while (negated p) s)))

(define (solve-by method f x0 a b)
  (show-all
   (map~ list->vector
         (select-by (λ (x) (< a (car x) b))
                    (iterations (method f) x0)))))

(require plot)
#;(time
 (plot 
 (list
  (function (λ (x) (exp (- (* x x 1/2)))) 0 2)
  (points
   (solve-by (euler 0.2) (λ (x y) (- (* x y))) '(0 1) 0 2)  #:sym #\*)
  (points
   (solve-by (runge-kutta 0.2) (λ (x y) (- (* x y))) '(0 1) 0 2)))))
  