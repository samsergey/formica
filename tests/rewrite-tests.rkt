#lang racket/base
(require "../rewrite.rkt"
         "../tools.rkt"
         rackunit)

(test-case
 "Simple tests."
 
 
 (check-equal? ((/. 'x --> 'y 
                    'y --> 'x) '(x + y))  '(y + x))
 
 
 ;"Использование условий."
 
 
 (check-equal? ((/. 'x --> (? #f) 'y 
                    'y --> 'x) '(x + y))  '(x + x))
 
 
 (check-equal? ((/. (list x y) --> (? (< x y)) 'less
                    (list x y) --> (? #t) 'stop
                    (list x y) --> 'never) '((2 1) (1 2) (5 6)))  '(stop less less))
 
 
 (check-equal? ((//. 'x --> (? #f) 'y 
                     'y --> 'x) '(x + y))  '(x + x))
 
 
 (check-equal? ((//. (list x y) --> (? (< x y)) 'less
                     (list x y) --> (? #t) 'stop
                     (list x y) --> 'never) '((2 1) (1 2) (5 6)))  '(stop less less))
 
 
 (check-equal? ((//. (list x y) -->. (? (< x y)) 'less
                     (list x y) -->. (? #t) 'stop
                     (list x y) --> 'never) '((2 1) (1 2) (5 6)))  '(stop less less))
 
 
 
 (check-equal? (map (curry apply (//. x y --> (? (< x y)) 'less
                                      x y --> (? #t) 'stop
                                      x y --> 'never)) '((2 1) (1 2) (5 6)))  '(stop less less)))


(struct N (x) #:transparent)
(struct 1+ (x) #:transparent)
(struct Sum (x y) #:transparent)
(struct Prod (x y) #:transparent)

(define calculate
  (rewrite-all-repeated 
   (N 0) --> 0
   (N n) --> (1+ (N (- n 1)))
   
   (Sum x 0) --> x
   (Sum x (1+ y)) --> (1+ (Sum x y))
   
   (Prod _ 0) --> 0
   (Prod x (1+ y)) --> (Sum x (Prod x y))
   
   (1+ x) --> (1+ (calculate x))
   (Sum x y) --> (Sum (calculate x) (calculate y))
   (Prod x y) --> (Prod (calculate x) (calculate y))))

(check-equal? (calculate (Prod (N 2) (Sum (N 2) (N 3))))
              (calculate (N 10)))

(test-case 
 "Arity of rewriting systems"
 (check-equal? (procedure-arity (rewrite 1 --> 2))  1)
  (check-equal? (procedure-arity (rewrite 1 --> 2 5 --> 3))  1)
  (check-equal? (procedure-arity (rewrite 1 2 --> 3))  2)
  (check-equal? (procedure-arity (rewrite 3 --> 6 1 2 --> 3))  '(1 2))
  (check-equal? (procedure-arity (rewrite x ... --> 4))  (arity-at-least 1))
  (check-equal? (procedure-arity (rewrite (? list? x) --> 7 x ... --> 4))  (arity-at-least 1))
  (check-equal? (procedure-arity (rewrite 3 --> 6 1 2 --> 3 1 2 x ... --> 4))  `(1 2 ,(arity-at-least 3)))
  (check-equal? (procedure-arity (rewrite-all 1 --> 2))  1)
  (check-equal? (procedure-arity (rewrite-all 1 --> 2 5 --> 3))  1)
  (check-equal? (procedure-arity (rewrite-all 1 2 --> 3))  2)
  (check-equal? (procedure-arity (rewrite-all 3 --> 6 1 2 --> 3))  '(1 2))
  (check-equal? (procedure-arity (rewrite-all x ... --> 4))  (arity-at-least 1))
  (check-equal? (procedure-arity (rewrite-all (? list? x) --> 7 x ... --> 4))  (arity-at-least 1))
  (check-equal? (procedure-arity (rewrite-all 3 --> 6 1 2 --> 3 1 2 x ... --> 4))  `(1 2 ,(arity-at-least 3)))
  (check-equal? (procedure-arity (rewrite-repeated 1 --> 2 2 --> 3))  1)
  (check-equal? (procedure-arity (rewrite-repeated 1 2 --> 3))  2)
  (check-equal? (procedure-arity (rewrite-repeated x ... --> 4))  (arity-at-least 1))
  (check-equal? (procedure-arity (rewrite-repeated (? list? x) --> 7 x ... --> 4))  (arity-at-least 1))
  (check-equal? (procedure-arity (rewrite-repeated 3 --> 6 1 2 --> 3 1 2 x ... --> 4))  `(1 2 ,(arity-at-least 3)))
  (check-equal? (procedure-arity (rewrite-all-repeated 1 --> 2))  1)
  (check-equal? (procedure-arity (rewrite-all-repeated 1 2 --> 3)) 2)
  (check-equal? (procedure-arity (rewrite-all-repeated x ... --> 4))  (arity-at-least 1))
  (check-equal? (procedure-arity (rewrite-all-repeated (? list? x) --> 7 x ... --> 4))  (arity-at-least 1))
  (check-equal? (procedure-arity (rewrite-all-repeated 3 --> 6 1 2 --> 3 1 2 x ... --> 4))  `(1 2 ,(arity-at-least 3)))
  (check-equal? (procedure-arity (rewrite-repeated 1 -->. 2))  1)
  (check-equal? (procedure-arity (rewrite-repeated 1 2 -->. 3))  2)
  (check-equal? (procedure-arity (rewrite-all-repeated 1 -->. 2))  1)
  (check-equal? (procedure-arity (rewrite-all-repeated 1 2 -->. 3))  2))

(require "../examples/rewrite.rkt")
(require "../examples/automata.rkt")
(require "../examples/infix.rkt")
(require "../examples/logics.rkt")
(require "../examples/turing.rkt")
(require "../examples/peano.rkt")
