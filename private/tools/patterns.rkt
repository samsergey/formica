#lang racket
;;______________________________________________________________
;;                   ______ 
;;                  (  //   _____ ____   .  __  __
;;                   ~//~ ((_)// // / / // ((_ ((_/_
;;                 (_//
;;..............................................................
;; Provides some patterns for match form
;;;=============================================================
(require racket/match
         (for-syntax racket/base))

(provide
 ; functional forms
 (rename-out [++ +])
 scons)

;;;-------------------------------------------------------------
;; increment expander
;;;-------------------------------------------------------------
(define-match-expander ++
  (λ (stx)
    (syntax-case stx ()
      [(++ a x) (cond
                  [(symbol? (syntax-e #'a)) #'(++ x a)]
                  [(not (symbol? (syntax-e #'x))) 
                   (raise-syntax-error 'match
                                       (format "pattern ~a is not an identifier" (syntax-e #'x))
                                       #'(++ a x))]
                  [(not (number? (syntax-e #'a)))
                   (raise-syntax-error 'match
                                       (format "increment ~a is not a number" (syntax-e #'a))
                                       #'(++ a x))]
                  [else #'(and (? number?) 
                               (app (λ (y) (- y a)) x))])]))
  (syntax-id-rules ()
    [(++ args ...) (+ args ...)]
    [++ +]))

;;;-------------------------------------------------------------
;; match-expander for streams
;;;-------------------------------------------------------------
(define-match-expander scons
  (syntax-rules ()
    [(scons x y) (and (? stream?) 
                      (not (? stream-empty?))
                      (app stream-first x)
                      (app stream-rest y))]))