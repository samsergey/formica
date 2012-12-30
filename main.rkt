#lang racket/base
;;______________________________________________________________
;;                   ______ 
;;                  (  //   _____ ____   .  __  __
;;                   ~//~ ((_)// // / / // ((_ ((_/_
;;                 (_//
;;..............................................................
(require racket/list
         racket/math
         racket/promise
         racket/string
         "tools.rkt"
         "partial-app.rkt")
(provide
 (except-out (all-from-out
              racket/base
              racket/list
              racket/math
              racket/promise
              racket/string
              "tools.rkt"
              "partial-app.rkt")
             procedure? lazy delay)
 (rename-out [procedure? function?]
             [lazy delay]))