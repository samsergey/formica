#lang racket/base
;;______________________________________________________________
;;                   ______ 
;;                  (  //   _____ ____   .  __  __
;;                   ~//~ ((_)// // / / // ((_ ((_/_
;;                 (_//
;;..............................................................
;; Provides formica/types module.
;;==============================================================
(require "private/types/types.rkt" )
(provide (all-from-out "private/types/types.rkt" ))

(require "private/types/ordering.rkt")
(provide 
 ordered?
 (contract-out
  (type-ordering (parameter/c (list: (cons: contract? Fun) ..)))
  (add-to-type-ordering (-> contract? (? contract? Fun) void?))
  (symbol<? (-> Sym Sym Sym .. Bool))
  (pair<? (-> pair? pair? pair? .. Bool))))

