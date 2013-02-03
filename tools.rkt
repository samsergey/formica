#lang racket/base
;;______________________________________________________________
;;                   ______ 
;;                  (  //   _____ ____   .  __  __
;;                   ~//~ ((_)// // / / // ((_ ((_/_
;;                 (_//
;;..............................................................
;; Provides formica/tools module.
;;==============================================================
(require "types.rkt")

(require "private/tools/arity.rkt")
(provide
 (contract-out
  (polyadic? predicate/c)
  (variadic? predicate/c)
  (fixed-arity? predicate/c)
  (nullary? predicate/c)
  (unary? predicate/c)
  (binary? predicate/c)
  (min-arity (-> Fun Nat))
  (max-arity (-> Fun (or/c Nat +inf.0)))))
 
(require "private/tools/curry.rkt")
(provide
 (contract-out
  (curry (-> Fun Any .. Any))
  (curryr (-> Fun Any .. Any))
  (curried (-> Fun Any .. Any))
  (r-curried (-> Fun Any .. Any))
  (curried? predicate/c)))

(require "private/tools/tools.rkt")
(provide
 ; functional forms
 ==>
 or and
 ; functions
 xor
 different? eq? equal? almost-equal? ≈ ≥ ≤
 (contract-out
  (any-args (-> unary? Fun))
  (all-args (-> unary? Fun))
  (tolerance (parameter/c Real))))

(require "private/tools/nest.rkt")
(provide
 (contract-out
  (composition (-> Fun Fun Fun .. Fun))
  (∘ (-> Fun Fun Fun .. Fun))
  (greedy? predicate/c)
  (greedy (-> Fun Fun))))

(require "private/tools/functionals.rkt")
(provide 
 id I1 I2 I3
 (contract-out
  (const (-> Any Fun))
  (negated (-> Fun Fun))
  (¬ (-> Fun Fun))
  (flipped (-> Fun Fun))
  (arg (-> Index Fun))
  (fif (-> Fun Fun Fun Fun))
  (andf (-> Fun .. Fun))
  (orf (-> Fun .. Fun))
  (argmap (-> Fun unary? Fun))
  (/@ (-> Fun unary? Fun))
  (fixed-point (->* (procedure?) (#:same-test (-> any/c any/c boolean?)) procedure?))))

(require "private/tools/tags.rkt")
(provide
 (contract-out
  (tagged? (-> Any Bool))
  (tag (-> tagged? Sym))
  (check-tag (-> Sym Any Bool))
  (set-tag ((symbol?) ((or/c #f symbol?)) . ->* . (procedure? . -> . tagged?)))
  (set-tag* ((symbol?) ((or/c #f symbol?)) . ->* . (procedure? . -> . procedure?)))))

(require "private/tools/patterns.rkt")
(provide +)
