#lang racket/base
;;______________________________________________________________
;;                   ______ 
;;                  (  //   _____ ____   .  __  __
;;                   ~//~ ((_)// // / / // ((_ ((_/_
;;                 (_//
;;..............................................................
;; Provides contract based type system.
;;==============================================================
(require racket/contract
         racket/contract/parametric
         (for-syntax racket/base))

(provide .->.
         (for-syntax parse-infix-contract))

;;;=================================================================
;;; helper functions and forms
;;;=================================================================
(define-for-syntax (parse-infix-contract stx) 
  (syntax-case stx (.->.)
    [(x ... .->. y) #`(.->. #,@(parse-infix-contract #'(x ...)) #,(parse-infix-contract #'y))]
    [(x y ...) #`(#,(parse-infix-contract #'x) #,@(parse-infix-contract #'(y ...)))]
    [x #'x]))

;; infix arrow which will replace the usual arrow in contracts
(define-syntax .->.
  (syntax-id-rules (.. ?)
    [(.->. v ... (? x ...) y .. res) (->* (v ...) (x ...) #:rest (listof y) res)]
    [(.->. v ... (? x ...) res) (->* (v ...) (x ...) res)]
    [(.->. v ... x .. res) (->* (v ...) #:rest (listof x) res)]
    [(.->. v ... res) (-> v ... res)]
    [.->. (raise-syntax-error '-> "could be used only in contract!")]))