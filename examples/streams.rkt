#lang formica/regular-app
(define-formal (delayed 1))
(define-syntax-rule (~ expr)
  (delayed (memoized (λ () expr))))
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

(define/c (until p) (foldr~ (λ (h t) (if (p h) '() (cons h t))) '()))
(define/c (skip-until p)
  (/. '() --> '()
      (cons h t) --> (if (p h) (cons h t) (skip-until p (! t)))))

(define (select-by p s) (until (negated p) (skip-until p s)))

(define nats (aryth 0 1))


(define (euler h f x y) 
  (+ y (* h (f x y))))

(define (F x y) (- (* x y)))
(define x (aryth 0 0.1))
(define y (cons~ 1 (map~ (curry euler 0.1 (verbose F)) x y)))







(define (rk2 h f x y)
  (+ y (* h (f (+ x (* h 1/2)) 
               (+ y (* h 1/2 (f x y)))))))


(define (solution method h f x0)
  (define x (aryth (first x0) h))
  (define y (cons~ (second x0) (map~ (curry method h f) x y)))
  (map~ list x y))


(define (dsolve f x0 a b (method euler) (h 0.1))
  (show-all
     (map~ list->vector
           (select-by (λ (x) (<= a (car x) b))
                      (solution method h f x0)))))


(define (verbose f)
  (λ x (displayln (apply (hold f) x))
    (apply f x)))
  



#;(require plot)

#;(time 
 (plot 
 (list
  (function (λ (x) (exp (- (* x x 1/2)))) 0 2)
  (points
   (dsolve F '(0 1) 0 2 euler 0.1)  #:sym #\*)
  (points
   (dsolve F '(0 1) 0 2 rk2 0.2)))))


(define (secant x-1 x-2 y-1 y-2)
  (/ (- (* y-1 x-2) (* y-2 x-1))
     (- y-1 y-2)))

(define (f x) (- (* x x) 2))

;(define xx (cons~ 1. (cons~ 2. (map~ secant xx (cdr~ xx) fx (cdr~ fx)))))
;(define fx (map~ (verbose f) xx))

(:: chars (port? -> (Stream char?))
  (define (chars p) 
    (until eof-object?
           (cons~ (read-char p) (chars p)))))

(:: words (port? -> (Stream Str))
  (define (words p) 
    (until eof-object? 
           (cons~ (read-word p) (words p)))))

(:: read-word (port? -> (∪ eof-object? Str))
  (define (read-word p)
    (let ([ch (chars p)])
      (if (empty? ch)
          eof
          (chars->string (select-by not-space? ch))))))

(define not-space? (negated (curry equal? #\space)))

(define (chars->string s) 
  (apply string (show s +inf.0)))
