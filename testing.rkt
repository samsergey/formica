#lang racket
;;______________________________________________________________
;;                   ______ 
;;                  (  //   _____ ____   .  __  __
;;                   ~//~ ((_)// // / / // ((_ ((_/_
;;                 (_//
;;..............................................................
;; Provides test syntax form
;;==============================================================
(require rackunit
         rackunit/text-ui
         "tools.rkt")

(provide (all-from-out rackunit)
         test
         ==>)

(define-syntax (test stx)
  (syntax-case stx (==> =error=>)
    [(_ expr ==> res rest ...) #'(begin (check almost-equal? expr res) (test rest ...))]
    [(_ expr =error=> exn rest ...) #'(begin (check-exn exn (λ () expr)) (test rest ...))]
    [(_ expr rest ...) (cond 
                         [(string? (syntax->datum #'expr)) 
                          #'(test-case expr (test rest ...))]
                         [else #'(begin (check-true (not (not expr))  (test rest ...)))])]
    [(_) #'(void)]))