#lang racket/base
;;______________________________________________________________
;;                   ______ 
;;                  (  //   _____ ____   .  __  __
;;                   ~//~ ((_)// // / / // ((_ ((_/_
;;                 (_//
;;..............................................................
;; Provides formica/partiall-app module.
;;==============================================================
(require "private/syntax/partial-app.rkt")
(provide
 (all-from-out "private/syntax/partial-app.rkt"))

(require "tacit.rkt")
(provide
 (all-from-out "tacit.rkt"))