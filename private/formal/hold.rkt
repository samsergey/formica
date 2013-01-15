#lang racket/base
;;______________________________________________________________
;;                   ______ 
;;                  (  //   _____ ____   .  __  __
;;                   ~//~ ((_)// // / / // ((_ ((_/_
;;                 (_//
;;..............................................................
; Provides the 'hold' function.
; This module is given outside the 'formal' module in order
; to be able to load it at expansion levels 0 an 1.
;;;=============================================================
(require racket/contract
         (for-syntax racket/base 
                     racket/match
                     "../tools/tags.rkt")
         "../tools/tags.rkt")

(provide 
 (contract-out
  (hold (->* ((or/c procedure? symbol?)) (procedure-arity?) formal-function?))
  ($ (->* ((or/c procedure? symbol?)) (procedure-arity?) formal-function?))
  (formal-function? predicate/c)))

;;--------------------------------------------------------------------------------
;; Returns a formal function with given name.
;;--------------------------------------------------------------------------------
(define hold
  (procedure-rename
  (case-lambda
    [(f) (cond
           [(procedure? f) (hold (or (object-name f) 'λ) (procedure-arity f))]
           [(symbol? f) (hold f (arity-at-least 0))])]
    [(f ar) (cond
              [(procedure? f) (hold (or (object-name f) 'λ) ar)]
              [(symbol? f) ((set-tag 'formal f) (procedure-reduce-arity (λ x (cons f x)) ar))])])
  'hold))

;;--------------------------------------------------------------------------------
;; A shorthand for hold function
;;--------------------------------------------------------------------------------
(define $ hold)

;;--------------------------------------------------------------------------------
;; A predicate for formal function
;;--------------------------------------------------------------------------------
(define (formal-function? x)
  (check-tag 'formal x))
                          