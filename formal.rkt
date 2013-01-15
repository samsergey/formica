#lang racket/base
;;______________________________________________________________
;;                   ______ 
;;                  (  //   _____ ____   .  __  __
;;                   ~//~ ((_)// // / / // ((_ ((_/_
;;                 (_//
;;..............................................................
;; Provides formica/formal module.
;;==============================================================
(require "types.rkt")

(require "private/formal/hold.rkt")
(provide 
 (contract-out
  (hold (-> (or/c Fun Sym) (? procedure-arity?) formal-function?))
  ($ (-> (or/c Fun Sym) (? procedure-arity?) formal-function?))
  (formal-function? predicate/c)))

(require "private/formal/formal.rkt")
(provide
 (contract-out
  (formals (parameter/c (listof predicate/c)))
  (formal? predicate/c)
  (n/f-list? predicate/c)
  (n/f-pair? predicate/c))
 define-formal
 formal
 formal-out)