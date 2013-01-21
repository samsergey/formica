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
         racket/string)

(provide
 (except-out (all-from-out
              racket/base
              racket/list
              racket/math
              racket/promise
              racket/string)
             procedure? lazy delay eval)
 (rename-out [procedure? function?]
             [lazy delay]
             [eval* eval]))

(require (except-in "tools.rkt" +)
         (except-in "rewrite.rkt" +)
         "partial-app.rkt"
         "formal.rkt"
         "tacit.rkt"
         "memoize.rkt"
         "types.rkt"
         "monad.rkt"
         "testing.rkt")
(provide
 (all-from-out "tools.rkt"
               "partial-app.rkt"
               "formal.rkt"
               "rewrite.rkt"
               "tacit.rkt"
               "memoize.rkt"
               "types.rkt"
               "monad.rkt"
               "testing.rkt"))