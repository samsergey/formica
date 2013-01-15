#lang racket/base
;;______________________________________________________________
;;                   ______ 
;;                  (  //   _____ ____   .  __  __
;;                   ~//~ ((_)// // / / // ((_ ((_/_
;;                 (_//
;;..............................................................
;; Provides formica/memoize module.
;;==============================================================
(require "types.rkt")

(require "private/tools/memoize.rkt")
(provide 
 define/memo
 define/memo/c
 (contract-out 
  (memoized (-> Fun memoized?))
  (memoized? predicate/c)))
