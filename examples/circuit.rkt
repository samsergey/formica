#lang formica
(:: sum ((Any -> Num) list? -> Num)
  (define (sum f lst)
    (apply + (map f lst))))

(:: bisection ((Real -> Real) Real Real -> (∪ Real #f))
  (define (bisection f a b)
    (and (<= (* (f a) (f b)) 0)
         (let ([c (* 0.5 (+ a b))])
           (if (< (abs (/ (- b a) c)) 1e-10)
               c
               (or (bisection f a c)
                   (bisection f c b)))))))

(define-type (R positive?))
(define-type (C positive?))
(define-type (L positive?))
(define-type --)
(define-type ==)
(define-type Circuit
  R? C? L?
  (--: Circuit ..)
  (==: Circuit ..))
(define-type ω positive?)
(define-type Imp complex?)

(:: impedance (Circuit -> (ω -> Imp))
  (define (impedance cir)
    (λ (w)
      (define/. Z
        (R r) --> r
        (C c) --> (/ -i c w)
        (L l) --> (* +i l w)
        (-- i ...) --> (sum Z i)
        (== i ...) --> (/ (sum / (map Z i))))
      (Z cir))))


(define (S c)
    (-- (R 10)
        (== (-- (R 3) 
                (L 0.5e-6))
            (C c))))

(:: resonance (Circuit positive? positive? -> positive?)
  (define/memo (resonance c w1 w2)
    (bisection (∘ imag-part (memoized (impedance c))) w1 w2)))

(require plot)

(time (plot (function (∘ angle (impedance (S 10e-9))) 1e6 50e6)))


