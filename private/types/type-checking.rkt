#lang racket/base
;;______________________________________________________________
;;                   ______ 
;;                  (  //   _____ ____   .  __  __
;;                   ~//~ ((_)// // / / // ((_ ((_/_
;;                 (_//
;;..............................................................
;; Provides contract based type system.
;;==============================================================
(require racket/contract
         (for-syntax racket/base))

(provide 
 is check-result check-argument check-type)

(define-for-syntax (parse-infix-contract stx) 
  (syntax-case stx (.->.)
    [(x ... .->. y) #`(.->. #,@(parse-infix-contract #'(x ...)) #,(parse-infix-contract #'y))]
    [(x y ...) #`(#,(parse-infix-contract #'x) #,@(parse-infix-contract #'(y ...)))]
    [x #'x]))

;;;=================================================================
;;; Safe type checking
;;;=================================================================
(define-syntax is
  (make-set!-transformer
   (lambda (stx)
     (syntax-case stx ()
       [(is x type) 
        (with-syntax ([c (parse-infix-contract #'type)])
          (syntax-protect
           #'(cond
               [(contract? c) 
                (with-handlers ([exn:fail? (lambda (exn) #f)])
                  (contract-first-order-passes? c x))]
               [else (raise-type-error 'is "predicate" 1 x type)])))]
       [is (syntax-protect
            #'(λ (x t) (is x t)))]))))

(define check-type (make-parameter #t))

(define-syntax-rule (check-result id type expr text ...)
  (let ([res expr])
    (if (or (not (check-type)) (is res type))
        res
        (raise-arguments-error 
         id 
         (format "the result should have type ~a"  (build-compound-type-name type))
         "received" res
         text ...))))

(define-syntax-rule (check-argument id type x text ...)
  (unless (or (not (check-type)) (is x type)) 
    (raise-arguments-error 
     id 
     (format "the argument should have type ~a" (build-compound-type-name type))
     "given" x
     text ...)))
