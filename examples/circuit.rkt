#lang formica


(define-type ω positive?)
(define-type Imp complex?)

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


(module symbolic formica
  (require "common")
         (define-formal 
           (R 1) (C 1) (L 1) || --)
         
         (define-type Circuit
           (R: positive?)
           (C: positive?)
           (L: positive?)
           (--: Circuit ..)
           (||: Circuit ..))
         
         (:: impedance (Circuit -> (ω -> Imp))
           (define (impedance cir)
             (λ (w)
               (define/. Z
                 (R r) --> r
                 (C c) --> (/ -i c w)
                 (L l) --> (* +i l w)
                 (-- i ...) --> (sum Z i)
                 (|| i ...) --> (/ (sum / (map Z i))))
               (Z cir)))))

(module data-driven formica
         
         (:: R (positive? ω -> Imp)
           (define (R r w) r))
         (:: C (positive? ω -> Imp)
           (define (C c w) (/ -i c w)))
         (:: L (positive? ω -> Imp)
           (define (L l w) (* +i l w)))
         
         (:: -- ((ω -> Imp) .. -> (ω -> Imp))
           (define (-- . fs)
             (λ (w) (sum (λ (f) (f w)) fs))))
         (:: || ((ω -> Imp) .. -> (ω -> Imp))
           (define (|| . fs)
             (λ (w) (/ (sum (λ (f) (/ (f w))) fs))))))

(require 'symbolic)
  
(define (S c)
    (-- (R 10)
        (|| (L 0.5e-6)
            (-- (R 3)
                (C c)))))


(:: resonance (Circuit positive? positive? -> positive?)
  (define/memo (resonance c w1 w2)
    (bisection (∘ imag-part (memoized (impedance c))) w1 w2)))

(require plot)

(time (plot (function (∘ magnitude (impedance (S 4e-9))) 1e6 5e7)))
(time (plot (function (λ (c) (resonance (S c) 1e5 1e9)) 1e-8 1e-11)))