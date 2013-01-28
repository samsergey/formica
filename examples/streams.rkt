#lang formica/regular-app

(define-formal (delayed 1) f)

(define-syntax-rule (delay expr)
  (delayed (memoized (lambda () expr))))

(define force
  (/. (delayed expr) --> (force (expr))))


(define-type (Stream A)
  '()
  (cons: A delayed?))

(define-syntax-rule (cons~ h t)
  (cons h (delay t)))

(:: car~ (pair? -> Any)
  (define (car~ s) (car s)))

(:: cdr~ (pair? -> Any)
 (define (cdr~ s) (force (cdr s))))


(define show
  (/. str    --> (show str 6)
      str  0 --> '()
      '() n --> '()
      (cons h t) n --> (cons h (show (force t) (- n 1)))))

(define/c (fold~ f x0)
  (/. '() --> x0
      (cons h t) --> (force (f h (delay (fold~ f x0 (force t)))))))

(define/c (any~ test?) (fold~ (∘ or test?) #f))

(define (stream  . x) (fold~ cons '() x))

(define/c (until p)
  (fold~ (λ (h t) (if (p h) '() (cons h t))) '()))

(define/c (skip-while p)
  (fold~ (λ (h t) (if (p h) t (cons h t))) '()))

(define/c (map~ f)
  (/. '() --> '()
      (cons h t) --> (cons~ (f h) (map~ f (force t)))
      s ... --> (cons~ (apply f (map car s))
                       (apply map~ f (map cdr~ s)))))

(define/c (filter~ p)
  (fold~ (λ (h t) (if (p h) (cons~ h t) t)) '()))


(define (in-producer g)
  (define (s . x) 
    (until eof-object? (cons~ (apply g x) (apply s x))))
  s)

(:: chars (port? -> (Stream char?))
  (define chars (in-producer read-char)))

(:: read-word (port? -> (∪ Str eof-object?))
  (define (read-word p)
    (let ([ch (chars p)])
      (if (empty? ch)
          eof
          (chars->string (until (curry equal? #\space) ch))))))

(:: chars->string ((Stream char?) -> Str)
  (define (chars->string s)
    (apply string (show s +inf.0))))

(:: words (port? -> (Stream Str))
  (define words (in-producer read-word)))

(:: enum (Num -> (Stream Num))
 (define (enum n)
   (cons~ n (enum (+ n 1)))))
      

(define p (open-input-string "one two three four eight nine ten"))
(define c (chars (open-input-string "one two three four")))
(define w (words (open-input-string "one two three four  eight nine ten")))

(define randoms (in-producer random))
(define ss (in-producer current-milliseconds))