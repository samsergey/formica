#lang racket/base
;;______________________________________________________________
;;                   ______ 
;;                  (  //   _____ ____   .  __  __
;;                   ~//~ ((_)// // / / // ((_ ((_/_
;;                 (_//
;;..............................................................
; Provides syntax for partial function application
;;;=============================================================
(require (for-syntax racket/base)
         "curry.rkt"
         "arity.rkt")

(provide
 using-partial-application
 (rename-out [#%app #%app*] ; regular application
             [%p-app #%app] ; partial application
             [apply apply*]
             [p-apply apply]
             ; fixing arity for some functions
             [p-map map]
             [p-ormap ormap]
             [p-andmap andmap]
             [p-+ +]
             [p-* *]))

(define using-partial-application
  (make-parameter #t))

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
                          (let* ([e (max-arity f)]
                                 [g (length (list args ...))])
                            (cond 
                              [(< g e) (#%app curry f args ...)]
                              [else (raise exn)])))])
         (#%app f args ...))]))


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

(define p-map  (procedure-reduce-arity map (arity-at-least 2)))
(define p-andmap  (procedure-reduce-arity andmap (arity-at-least 2)))
(define p-ormap  (procedure-reduce-arity ormap (arity-at-least 2)))
(define p-+  (procedure-reduce-arity + (arity-at-least 2)))
(define p-*  (procedure-reduce-arity * (arity-at-least 2)))