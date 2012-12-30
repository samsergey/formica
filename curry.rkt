#lang racket/base
;;______________________________________________________________
;;                   ______ 
;;                  (  //   _____ ____   .  __  __
;;                   ~//~ ((_)// // / / // ((_ ((_/_
;;                 (_//
;;..............................................................
; Provides curried and r-curried functions.
;;;=============================================================
(require "tags.rkt"
         "arity.rkt"
         (only-in racket/list argmin argmax)
         racket/contract)

(provide 
 (contract-out
  (curry (->* (procedure?) #:rest list? any/c))
  (curryr (->* (procedure?) #:rest list? any/c))
  (curried (->* (procedure?) #:rest list? any/c))
  (r-curried (->* (procedure?) #:rest list? any/c))
  (curried? predicate/c)))

;;;-------------------------------------------------------------
;;; helper functions
;;;-------------------------------------------------------------
(define-syntax-rule (proc-test name f)
  (unless (procedure? f) (raise-type-error 'name "procedure" f)))

(define (maximal-arity f)
  (let ([arity (procedure-arity f)])
    (cond [(integer? arity) arity]
          [(arity-at-least? arity) #f]
          [(ormap arity-at-least? arity) #f]
          [else (apply max arity)])))

(define (mark f)
  (if (curried? f) 
      (λ (p) (procedure-rename p (or (object-name f) 'λ)))
      (set-tag 'curried (or (object-name f) 'λ))))

;;; Adopted from racket/functions.
(define (make-curry right?)
  (define (curry* f args)   
    (let* ([arity (procedure-arity f)]
           [max-ar (maximal-arity f)]
           [min-ar (min-arity f)]
           [n (length args)])
      (define (loop args n)
        (cond
          [(procedure-arity-includes? f n) (apply f args)]
          [(and max-ar (< max-ar n))  (apply raise-arity-error f arity args)]
          [else (λ more 
                  (loop (if right? 
                            (append more args) 
                            (append args more))
                        (+ n (length more))))]))
      
      (cond
        [(equal? n max-ar) (apply f args)]
        [(and max-ar (> n max-ar))  (apply raise-arity-error f arity args)]
        [else (λ more 
                (loop (if right? 
                          (append more args) 
                          (append args more))
                      (+ n (length more))))])))
  
  (case-lambda 
    [(f) ((mark f)
          (procedure-reduce-arity
           (λ args (let ([cf (curry* f '())])
                     (if (procedure? cf)
                         (apply cf args)
                         cf)))
           (procedure-arity f)))]
    [(f . args) (let ([res (curry* f args)])
                  (if (procedure? res)
                      ((mark f)
                       (procedure-reduce-arity 
                        res 
                        (reduce-arity (procedure-arity f) (length args))))
                      res))]))

;;;-------------------------------------------------------------
;;; curry
;;;-------------------------------------------------------------
(define curry (procedure-rename (make-curry #f) 'curry))
(define curried curry)
;;;-------------------------------------------------------------
;;; curryr
;;;-------------------------------------------------------------
(define curryr (procedure-rename (make-curry #t) 'curryr))
(define r-curried curryr)
;;;-------------------------------------------------------------
;;; curried?
;;;-------------------------------------------------------------
(define (curried? x) 
  (and (procedure? x)
       (check-tag 'curried x)))

