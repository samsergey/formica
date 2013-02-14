#lang racket/base
;;______________________________________________________________
;;                   ______ 
;;                  (  //   _____ ____   .  __  __
;;                   ~//~ ((_)// // / / // ((_ ((_/_
;;                 (_//
;;..............................................................
;; Provides formica/monad module.
;;==============================================================
(require "types.rkt" "tools.rkt" "formal.rkt")
(require "private/monad/base.rkt")
(provide 
 monad
 ; forms
 define-monad
 >>= >> <- <-: <<- <<-:
 do
 collect
 using
 check-result
 ; functional forms
 type
 return
 bind
 mzero
 mplus
 failure
 lift/m
 compose/m
 undefined
 ;functions
 (contract-out 
  (monad? predicate/c)
  (monad-zero? predicate/c)
  (monad-plus? predicate/c)
  #;(using-monad (parameter/c monad?))
  (lift (-> Fun Fun))
  (fold/m (-> binary? Any list? Any))
  (filter/m (-> unary? list? Any))
  (listable? Type)
  (sequence/m (-> listable? Any))
  (map/m (-> unary? listable? Any))
  (sum/m (-> list? Any))
  (guard (-> Any Any))
  (guardf (-> unary? unary?))
  (Id monad?)))


(require "private/monad/sequential-monads.rkt"
         racket/set
         racket/stream
         racket/list)
(provide 
 amb

 zip
 (contract-out 
  ; Monoid
  (Monoid (->* (#:return (-> Any .. listable?)
                  #:mplus (-> listable? listable? listable?)) 
                 (#:map (-> (-> Any listable?) listable? listable?)) monad-plus?))
  (mplus-map (-> (-> Any listable?) listable? Any))

  ; List monad
  (List monad-plus?)  
  (concatenate (-> listable? .. n/f-list?))
  (concat-map (-> (-> Any n/f-list?) listable? n/f-list?))
  ; Stream monad
  (Stream monad-plus?)
  (stream-concat-map (-> (-> Any stream?) listable? stream?)) 
  (stream-concatenate (-> listable? listable? stream?))  
  (stream-take (-> stream? Index list?))
  ; Amb monad
  (Amb monad-plus?)
  (amb-union-map (-> (-> Any stream?) listable? stream?))
  (amb-union (-> listable? listable? stream?)))

 (all-from-out racket/set
               racket/stream
               racket/list))

(require "private/monad/inferrence.rkt")
(provide 
 (contract-out 
  (Inferred monad-plus?))
  (rename-out (using-monad* using-monad)))

(using-monad* List Stream)