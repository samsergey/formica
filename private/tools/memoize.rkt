#lang racket/base
;;______________________________________________________________
;;                   ______ 
;;                  (  //   _____ ____   .  __  __
;;                   ~//~ ((_)// // / / // ((_ ((_/_
;;                 (_//
;;..............................................................
;; Provides memoization utilities.
;;==============================================================
(require (for-syntax racket/base)
         racket/contract
         "tags.rkt"
         (only-in "arity.rkt" inherit-arity)
         "../../tacit.rkt")

;;;==============================================================
;;;  экспорт
;;;==============================================================
(provide 
 define/memo
 define/memo/c
 (contract-out 
  (memoized (-> procedure? memoized?))
  (memoized? predicate/c)))

;;;==============================================================
;;;  реализация
;;;==============================================================

;; returns memoized version of function f
(define (memoized f)
  (cond
    [(memoized? f) f]
    [else (let ([table (make-hash)]
                [name (or (object-name f) 'λ)])
            ((set-tag 'memoized name)
             ((inherit-arity f) 
              (λ vars
                (or (hash-ref table vars #f)
                    (let ([res (apply f vars)])
                      (hash-set! table vars res)
                      res))))))]))

;; the predicate for memoized functions
(define memoized?
  (flat-named-contract 
   'memoized? 
   (λ (x) (check-tag 'memoized x))))

;; the definition of memoized fonctions
(define-syntax (define/memo stx)
  (syntax-case stx (lambda)
    
    [(define/memo (id var ...) body ...) 
     #'(define/memo id 
         (lambda (var ...) body ...))]
    
    [(define/memo (id x ... . vars) body ...) 
     #'(define/memo id 
         (lambda (x ... . vars) body ...))]
    
    [(define/memo id (lambda vars body ...)) 
     #'(define id 
         (memoized 
          (procedure-rename 
           (lambda vars body ...) 
           'id)))]
    
    [(define/memo id form) 
     (with-syntax ([idx #'id] [formx #'form])
       #'(begin
           (unless (procedure? form) (raise-syntax-error 'define/memo "the right-hand side of definion is not a function" #'form))
           (define idx (memoized formx))))]
    
    [(define/memo id form ...) 
     (raise-syntax-error 'define/memo "bad syntax (multiple expressions after identifier)")]
    ))

;; the tacit definition of memoized fonctions
(define-syntax (define/memo/c stx)
  (syntax-case stx ()
    [(define/memo/c (id spec ...) body) 
     (with-syntax ([(fid) (generate-temporaries #'(id))])
       #'(begin
           (define/c (fid spec ...) body)
           (define id (procedure-rename (memoized fid) 'id))))]))
   

