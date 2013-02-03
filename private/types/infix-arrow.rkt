#lang racket/base
;;______________________________________________________________
;;                   ______ 
;;                  (  //   _____ ____   .  __  __
;;                   ~//~ ((_)// // / / // ((_ ((_/_
;;                 (_//
;;..............................................................
;; Provides the infix arrow used in contracts
;;==============================================================
(require racket/contract
         racket/contract/parametric
         (for-syntax racket/base)
         "type-checking.rkt")

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
    [(.->. v ... (? x ...) y .. res) (->* ((safe v) ...) ((safe x) ...) #:rest (listof (safe y)) (safe res))]
    [(.->. v ... (? x ...) res) (->* ((safe v) ...) ((safe x) ...) (safe res))]
    [(.->. v ... x .. res) (->* ((safe v) ...) #:rest (listof (safe x)) (safe res))]
    [(.->. v ... res) (-> (safe v) ... (safe res))]
    [.->. (raise-syntax-error '-> "could be used only in contract!")]))

(define-syntax-rule (safe x) x
  #;(flat-named-contract (build-compound-type-name x) #;(or (object-name x) x) (λ (y) (is y x))))