#lang formica
(define-formal 
  (R 1) (C 1) (L 1) || --)

(define-type Circuit
  (R: positive?)
  (C: positive?)
  (L: positive?)
  (--: Circuit ..)
  (||: Circuit ..))


(define (sum f lst) (apply + (map f lst)))

(:: impedance (Circuit -> (positive? -> Num))
  (define (impedance circ)
    (λ (w)
      (define/. Z
        (R r) --> r
        (C c) --> (/ -i c w)
        (L l) --> (* +i l w)
        (-- c ...) --> (sum Z c)
        (|| c ...) --> (/ (sum 1/Z c)))
      (define (1/Z x) (/ (Z x)))
      (Z circ)))

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

(define cir (-- (R 10)
                (|| (L 0.5)
                    (-- (R 3)
                        (C 1e-5)))))

(require plot)
(time (plot (function (∘ imag-part (simpedance cir)) 100 1000)))
(time (plot (function (∘ imag-part (impedance cir)) 100 1000)))