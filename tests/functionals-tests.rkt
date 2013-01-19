#lang racket/base
(require "../tools.rkt"
         "test-utils.rkt")

(test-case
 "id tests"
 (check-equal? (procedure-arity id) 1))

(test-case
 "arg tests"
 (for ([n '(1 2 3 4)])
   (check-equal? (procedure-arity (arg n)) (arity-at-least n) "Arity test")
   (check-equal? ((arg n) 1 2 3 4 5) n "Functionality test")))

(test-case
 "negated tests"
 (for ([f test-functions])
   (check-equal? (procedure-arity (negated f)) 
                 (procedure-arity f) "Arity test"))
 
 (for ([f (list odd? even? = > <)])
   (for ([args (sufficient-argument-list f)])
     (check-equal? (apply (negated f) args)
                   (not (apply f args)) "Functionality test"))))

(test-case
 "flipped tests"
 (for ([f test-functions])
   (check-equal? (procedure-arity (flipped f)) 
                 (procedure-arity f) "Arity test"))
 
 (for ([f test-functions])
   (for ([args (sufficient-argument-list f)])
     (check-equal? (apply (flipped f) (reverse args))
                   (apply f args) "Functionality test"))))

(test-case
 "fif tests"
 (for ([f test-functions])
   (check-equal? (procedure-arity (fif f f f)) 
                 (procedure-arity f) "Arity test"))
 (check-equal? (map (fif negative? add1 sub1) '(-2 -1 1 2))
               '(-1 0 0 1) "Unary functions")
 (check-equal? (map (fif < + -) '(1 2 3 4) '(3 2 5 1))
               '(4 0 8 3) "Binary functions")
 (check-equal? (andf u) u)
 (check-true ((andf) 'x)))

(test-case
 "andf tests"
 (for ([f test-functions])
   (check-equal? (procedure-arity (andf f f)) 
                 (procedure-arity f) "Arity test")
   (check-equal? (procedure-arity (andf f f f)) 
                 (procedure-arity f) "Arity test"))
 (check-equal? (filter (andf integer? positive?) '(1 -3 1.2 s 0 "a")) '(1))
 (check-equal? (map (andf < -) '(1 -3 1.2) '(2 4 -2)) '(-1 -7 #f))
 (check-equal? (orf u) u)
 (check-false ((orf) 'x)))

(test-case
 "orf tests"
 (for ([f test-functions])
   (check-equal? (procedure-arity (orf f f)) 
                 (procedure-arity f) "Arity test")
   (check-equal? (procedure-arity (orf f f f)) 
                 (procedure-arity f) "Arity test"))
 (check-equal? (filter (orf integer? symbol?) '(1 -3 1.2 s 0 "a")) '(1 -3 s 0))
 (check-equal? (map (orf < -) '(1 -3 1.2) '(2 4 -2)) '(#t #t 3.2)))

(test-case
 "argmap tests"
 (check-equal? ((/@ u u) 'x) '(u (u x)))
 (check-equal? ((/@ b u) 'x 'y) '(b (u x) (u y)))
 (check-equal? ((/@ t u) 'x 'y 'z) '(t (u x) (u y) (u z)))
 (check-equal? ((/@ t u*) 'x 'y 'z) '(t (u* x) (u* y) (u* z)))
 (check-equal? ((/@ b v) 'x 'y) '(b (v x) (v y)))
 (check-equal? ((/@ b o) 'a 'b) '(b (o a 88) (o b 88)))
 (check-equal? ((/@ b n*) 'x 'y) '(b (n* x) (n* y)))
 (check-exn exn:fail:contract? (λ () (/@ b b))))

(test-case
 "fixed-point tests"
 (check-equal? ((fixed-point id) 'x) (id 'x))
 (let* ([F (λ (x y)
             (if (<= x 1) (values x y) (values (- x 1) (* x y))))]
        [fact (compose list (fixed-point F))])
   (check-equal? (fact 4 1) '(1 24)))
 (let ([fcos (fixed-point cos)]) 
   (check-equal? (fcos 1) (cos (fcos 1))))
 (let* ([Newt-step (λ (f df) (λ (x) (- x (/ (f x) (df x)))))]
        [Sqrt (λ (y) (fixed-point (Newt-step (λ (x) (- (* x x) y)) (λ (x) (* 2. x)))
                                  #:same-test almost-equal?))])
   (check almost-equal? (sqrt 2) ((Sqrt 2.) 1.)))
 (for ([f test-functions])
   (check-equal? (procedure-arity (fixed-point f)) (procedure-arity f))))

