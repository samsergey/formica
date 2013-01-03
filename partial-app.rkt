#lang racket/base
;;______________________________________________________________
;;                   ______ 
;;                  (  //   _____ ____   .  __  __
;;                   ~//~ ((_)// // / / // ((_ ((_/_
;;                 (_//
;;..............................................................
; Provides syntax for partial function application
;;;=============================================================
(require (for-syntax racket/base racket/contract)
         racket/contract
         "curry.rkt"
         "arity.rkt")

(provide
 (rename-out [#%app #%app*] ; regular application
             [%p-app #%app] ; partial application
             [apply apply*] ; regular application
             [p-apply apply] ; partial application
             [p-map map]
             [p-ormap ormap]
             [p-andmap andmap]
             [p-+ +]
             [p-* *]))

;; partial application form
;; Catches the exn:fail:contract:arity? exception and 
;; returns partially applyed function if arity allows. 
(define-syntax (%p-app stx)
  (syntax-case stx (apply curry curryr)
    [(%p-app f) #'(with-handlers ([exn:fail:contract:arity? 
                                   (lambda (exn) (#%app curry f))])
                    (f))]
    
    [(%p-app curry f args ...) #'(#%app curry f args ...)]
    [(%p-app curryr f args ...) #'(#%app curryr f args ...)]
    
    [(%p-app f args ...) 
     #'(with-handlers ([exn:fail:contract:arity? 
                        (lambda (exn)
                          (let ([e (max-arity f)]
                                [g (length (list args ...))])
                            (cond 
                              [(< g e) (#%app curry f args ...)]
                              [else (raise exn)])))]
                       [exn:fail:contract:blame? 
                        (lambda (exn)
                          (if (regexp-match #rx"Signature violation:  received [0-9]+ argument" (exn-message exn))
                              (let* ([e (max-arity f)]
                                     [g (length (list args ...))])
                                (cond 
                                  [(< g e) (#%app curry f args ...)]
                                  [else (raise exn)]))
                              (raise exn)))])
         (#%app f args ...))]))


;; partial application operator
(define-syntax p-apply
  (syntax-id-rules ()
    [(p-apply f) (#%app curry apply f)]
    [(p-apply f args ...) 
     (with-handlers ([exn:fail:contract:arity? 
                        (lambda (exn)
                          (let* ([e (max-arity f)]
                                 [g (length (list* args ...))])
                            (cond 
                              [(< g e) (#%app apply curry f args ...)]
                              [else (raise exn)])))])
         (#%app apply f args ...))]
    [p-apply apply]))

;; reducing arity for some functions
(define p-map  (procedure-reduce-arity map (arity-at-least 2)))
(define p-andmap  (procedure-reduce-arity andmap (arity-at-least 2)))
(define p-ormap  (procedure-reduce-arity ormap (arity-at-least 2)))
(define p-+  (procedure-reduce-arity + (arity-at-least 2)))
(define p-*  (procedure-reduce-arity * (arity-at-least 2)))