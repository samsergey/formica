#lang formica

(define-formal (delayed 1) f)

(define-syntax-rule (delay expr)
  (delayed (memoized (lambda () expr))))

(:: car~ (pair? -> Any)
  (define (car~ s) (car s)))

(:: cdr~ (pair? -> Any)
 (define (cdr~ s) (force (cdr s))))

(define force
  (/. (delayed expr) --> (force (expr))))


(define-type (Stream A)
  '()
  (cons: A delayed?))

(define-syntax-rule (cons~ h t)
  (cons h (delay t)))

(define show
  (/. str    --> (show str 6)
      str  0 --> '()
      '() n --> '()
      (cons h t) n --> (cons h (show (force t) (- n 1)))))

(:: until ((Any -> Bool) (Stream Any) -> (Stream Any))
  (define/c (until p)
    (/. '() --> '()
        (cons h t) --> (if (p h)
                           '()
                           (cons~ h (until p (force t)))))))

(define/c (map~ f)
  (/. '() --> '()
      (cons h t) --> (cons~ (f h) (map~ f (force t)))
      s ... --> (cons~ (apply f (map car s))
                       (apply map~ f (map cdr~ s)))))

(:: chars (port? -> (Stream char?))
  (define (chars p) 
    (until eof-object? 
           (cons~ (read-char p) (chars p)))))

(:: words (port? -> (Stream Str))
  (define (words p) 
    (until (equal? "")
           (cons~ (chars->string (until (equal? #\space) 
                                        (chars p)))
                  (words p)))))

(:: chars->string ((Stream char?) -> Str)
  (define (chars->string  s)
    (apply string (show s +inf.0))))

(:: enum (Num -> (Stream Num))
 (define (enum n)
   (cons~ n (enum (+ n 1)))))

(define/c (filter~ p)
  (/. '() --> '()
      (cons h t) --> (if (p h)
                         (cons~ h (filter~ p (force t)))
                         (filter~ p (force t)))))
      

(define c (chars (open-input-string "one two three four")))
(define w (words (open-input-string "one two three four")))

