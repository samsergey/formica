#lang racket/base
;;______________________________________________________________
;;                   ______ 
;;                  (  //   _____ ____   .  __  __
;;                   ~//~ ((_)// // / / // ((_ ((_/_
;;                 (_//
;;..............................................................
;; Provides tacit definitions.
;;==============================================================
(require (for-syntax racket/base)
         "../../rewrite.rkt"
         (only-in "../tools/arity.rkt" reduce-arity))

(provide define/c)

; FIXME (define/c (f x) 'sin) does not define f nor rases an error

;;; ==============================================================
;;;  helper functions
;;; ==============================================================
(define (arity-add fa a)
  (cond
    [(list? fa) (map (λ (x) (arity-add x a)) fa)]
    [(integer? fa) (+ fa a)]
    [(arity-at-least? fa) (arity-at-least (+ (arity-at-least-value fa) a))]))

(define-syntax (define/c/. stx)
  (syntax-case stx ()
    [(_ (f x ...) (R y ...)) 
     (with-syntax ([(ar ...) (rules-arity (parse-multiary-rules 'null #'(y ...)))])
       #'(define f
           (procedure-rename
            (procedure-reduce-arity
             (λ (x ... . z) (apply (R y ...) z))
             (arity-add (list ar ...) (length '(x ...))))
            'f)))]))

(define-syntax (define/c//. stx)
  (syntax-case stx ()
    [(_ (f x ...) (R y ...)) 
     (with-syntax ([(ar ...) (rules-arity (parse-multiary-rules 'null #'(y ...)))])
       #'(define f
           (procedure-rename
            (procedure-reduce-arity
             (λ (x ... . z) (apply (R y ...) z))
             (arity-add (list ar ...) (length '(x ...))))
            'f)))]))

(define ((inherit-name f) g)
  (procedure-rename 
   g
   (cond
     [(procedure? f) (object-name f)]
     [(symbol? f) f]
     [else (object-name g)])))

(define-for-syntax (bad-rhs stx)
  (raise-syntax-error 'define/c 
                      "the right-hand side of point-free definition is not a function constructor, nor the function call"
                      stx))

;;;=============================================================
;;; point-free definitions
;;;=============================================================
(define-syntax (define/c stx)
  (syntax-case stx (match-lambda match-lambda* case-lambda lambda λ 
                                 /. //. replace replace-all
                                 replace-repeated replace-all-repeated 
                                 quote quasiquote begin)
    [(_ (f x ... (a ...)) body) 
     (raise-syntax-error #f
                         "default or key values in point-free definition"
                         #'(define/c (f x ... (a ...)) body))]
    
    [(_ (f x ...) (/. y ...)) 
     #'(define/c/. (f x ...) (/. y ...))]
    
    [(_ (f x ...) (//. y ...)) 
     #'(define/c//. (f x ...) (//. y ...))]
    
    [(_ (f x ...) (replace y ...)) 
     #'(define/c/. (f x ...) (replace y ...))]
    
    [(_ (f x ...) (replace-all y ...)) 
     #'(define/c/. (f x ...) (replace-all y ...))]
    
    [(_ (f x ...) (replace-repeated y ...)) 
     #'(define/c//. (f x ...) (replace-repeated y ...))]
    
    [(_ (f x ...) (replace-all-repeated y ...)) 
     #'(define/c//. (f x ...) (replace-all-repeated y ...))]
    
    [(_ (f x ...) (lambda () body ...)) 
     #'(define (f x ...) body ...)]
    
    [(_ (f x ...) (lambda (y ...) body ...)) 
     #'(define (f x ... y ...) body ...)]
    
    [(_ (f x ...) (lambda y body ...)) 
     #'(define (f x ... . y) body ...)]
    
    [(_ (f x ...) (λ var body ...)) 
     #'(define/c (f x ...) (lambda var body ...))]
    
    [(_ (f x ...) (match-lambda
                    [y body] ...)) 
     #'(define f 
         ((inherit-name 'f)
          (match-lambda* 
            [(list x ... y) body] ...)))]
    
    [(_ (f x ...) (match-lambda* 
                    [(list y ...) body] ...)) 
     #'(define f 
         ((inherit-name 'f)
          (match-lambda* 
            [(list x ... y ...) body] ...)))]
    
    [(_ (f x ...) (case-lambda
                    [(y ...) body ...] ...)) 
     #'(define f 
         ((inherit-name 'f)
          (case-lambda 
            [(x ... y ...) body ...] ...)))]
    
    [(_ (f x ...) (case-lambda
                    [(y ...) body ...] ... [z rest-body ...])) 
     #'(define f 
         ((inherit-name 'f)
          (case-lambda 
            [(x ... y ...) body ...] ...
            [(x ... . z) rest-body ...])))]
    
    ;[(_ f (quote b ...)) (bad-rhs stx)]
    
    ;[(_ f (quasiquote b ...)) (bad-rhs stx)]
    
    ;[(_ f (begin b ...)) (bad-rhs stx)]
    
    [(_ (f x ...) (g y ...)) #'(begin
                                 (unless (procedure? g) 
                                   (raise-syntax-error 
                                    'define/c 
                                    "the right-hand side of definiton is not a function application"
                                    #'(g y ...)))
                                 (define f 
                                   ((inherit-name 'f)
                                    (procedure-reduce-arity
                                     (λ (x ... . r)
                                       (apply g (append (list y ...) r)))
                                     (arity-add (reduce-arity (procedure-arity g) (length '(y ...)))
                                                (length '(x ...)))))))]
    
    [(_ f b) (if (not (symbol? (syntax-e #'b)))
                 (bad-rhs stx)
                 #'(begin 
                     (unless (procedure? b) 
                       (raise-syntax-error 
                                    'define/c 
                                    "the right-hand side of definiton is not a function nor a function application"
                                    #'b))
                     (define/c f (b))))]
    
    [(_ f a b ...) (bad-rhs stx)]))
