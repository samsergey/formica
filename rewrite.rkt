#lang racket/base
;;______________________________________________________________
;;                   ______ 
;;                  (  //   _____ ____   .  __  __
;;                   ~//~ ((_)// // / / // ((_ ((_/_
;;                 (_//
;;..............................................................
;; Provides formica/rewrite module.
;;==============================================================
(require "rewrite/rewrite.rkt"
         "rewrite/rewrite-repeated.rkt")
 
(provide (all-from-out "rewrite/rewrite.rkt"
                       "rewrite/rewrite-repeated.rkt")
         (protect-out --> -->.))

(define-syntax -->
  (syntax-id-rules ()
    (--> (raise-syntax-error '--> "could be used only in rewriting rules."))))

(define-syntax -->.
  (syntax-id-rules ()
    (-->. (raise-syntax-error '-->. "could be used only in rewriting rules."))))