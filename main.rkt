#lang racket/base
;;______________________________________________________________
;;                   ______ 
;;                  (  //   _____ ____   .  __  __
;;                   ~//~ ((_)// // / / // ((_ ((_/_
;;                 (_//
;;..............................................................
(define-namespace-anchor anch)
(define formica-namespace (namespace-anchor->namespace anch))
(define eval*
  (case-lambda
    [(expr) (eval expr formica-namespace)]
    [(expr n) (eval expr n)]))

(require racket/list
         racket/math
         racket/promise
         racket/string
         "tools.rkt"
         "partial-app.rkt"
         "formal.rkt"
         "rewrite.rkt"
         "tacit.rkt"
         "memoize.rkt"
         "types.rkt"
         "monad.rkt")
(provide
 (except-out (all-from-out
              racket/base
              racket/list
              racket/math
              racket/promise
              racket/string
              "tools.rkt"
              "partial-app.rkt"
              "formal.rkt"
              "rewrite.rkt"
              "tacit.rkt"
              "memoize.rkt"
              "types.rkt"
              "monad.rkt")
             procedure? lazy delay eval)
 (rename-out [procedure? function?]
             [lazy delay]
             [eval* eval]))

