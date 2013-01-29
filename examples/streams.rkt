#lang formica
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

(define/c (any p) (foldr (∘ or p) #f))

#;(define/c (map~ f) (foldr~ (∘ cons f) '()))

(define (map~ f  . s)
  (if (any empty? s) 
      '()
      (cons~ (apply f (map car s))
             (apply map~ f (map cdr~ s)))))

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

(define/c (take-while p) (foldr~ (λ (h t) (if (p h) (cons h t) '())) '()))
(define/c (skip-until p) (foldr~ (λ (h t) (if (not (p h)) t (cons h t))) '()))
(define (select-by p s) (skip-until p (take-while p s)))

(define nats (aryth 0 1))






























(define (euler h f x y) 
  (+ y (* h (f x y))))

(define (rk2 h f x y)
  (+ y (* h (f (+ x (* h 1/2)) 
               (+ y (* h 1/2 (f x y)))))))

(define x (aryth 0 0.1))
(define (F x y) (- (* x y)))
(define y (cons~ 1 (map~ (curry rk2 0.1 F) x y)))





(define (solution method h f x0)
  (define x (aryth (first x0) h))
  (define y (cons~ (second x0) (map~ (method h f) x y)))
  (map~ list x y))


(define (dsolve f x0 a b (method euler) (h 0.1))
  (show-all
     (map~ list->vector
           (select-by (λ (x) (<= a (car x) b))
                      (solution method h f x0)))))



(require plot)

(time 
 (plot 
 (list
  (function (λ (x) (exp (- (* x x 1/2)))) 0 2)
  (points
   (dsolve F '(0 1) 0 2 euler 0.1)  #:sym #\*)
  (points
   (dsolve F '(0 1) 0 2 rk2 0.2)))))

  