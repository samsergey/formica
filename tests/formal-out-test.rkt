#lang racket/base
(require "../formal.rkt")

;;--------------------------------------------------------------------------------
;; The tests for the formal-out form
;;--------------------------------------------------------------------------------
(define-formal 
  %a-provide-test% 
  %another-provide-test%)

(provide (formal-out 
          %a-provide-test% 
          %another-provide-test%))