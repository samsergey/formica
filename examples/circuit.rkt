#lang formica
(define-formal 
  (R 1) (C 1) (L 1) || --)

(define-type Circuit
  (R: positive?)
  (C: positive?)
  (L: positive?)
  (--: Circuit ..)
  (||: Circuit ..))


(:: simpedance (Circuit -> (positive? -> Num))
  (define (simpedance circ)
    (define/. Z
      (R r) --> r
      (C c) --> `(/ -i ,c w)
      (L l) --> `(* +i ,l w)
      (-- c ...) --> `(+ ,@(map Z c))
      (|| c ...) --> `(/ (+ ,@(map 1/Z c))))
    (define (1/Z x) `(/ ,(Z x)))
    (eval `(λ (w) ,(Z circ)))))

(:: impedance (Circuit -> (positive? -> complex?))
  (define (impedance cir)
    (λ (w)
      (define/. Z
        (R r) --> r
        (C c) --> (/ -i c w)
        (L l) --> (* +i l w)
        (-- i ...) --> (sum Z i)
        (|| i ...) --> (/ (sum / (map Z i))))
      (Z cir))))

(:: sum ((Any -> Num) list? -> Num)
  (define (sum f lst)
    (apply + (map f lst))))


(define (S c)
  (-- (R 10)
      (|| (L 0.5e-6)
          (-- (R 3)
              (C c)))))

(:: bisection ((Real -> Real) Real Real -> (∪ Real #f))
  (define (bisection f a b)
    (and (<= (* (f a) (f b)) 0)
         (let ([c (* 0.5 (+ a b))])
           (if (< (abs (/ (- b a) c)) 1e-10)
               c
               (or (bisection f a c)
                   (bisection f c b)))))))

(:: resonance (Circuit positive? positive? -> positive?)
  (define/memo (resonance c w1 w2)
    (bisection (∘ imag-part (memoized (impedance c))) w1 w2)))

(require plot)

(time (plot (function (∘ magnitude (impedance (S 4e-9))) 1e6 5e7)))
(time (plot (function (λ (c) (resonance (S c) 1e5 1e9)) 1e-8 1e-11)))