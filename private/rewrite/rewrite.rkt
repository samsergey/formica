#lang racket/base
;;______________________________________________________________
;;                   ______ 
;;                  (  //   _____ ____   .  __  __
;;                   ~//~ ((_)// // / / // ((_ ((_/_
;;                 (_//
;;..............................................................
;; Provides rewrite form.
;;==============================================================
(require racket/match
         (for-syntax racket/base))

(provide rewrite
         rewrite-all /.
         define/.)


;;====================================================
;; error messages
;;====================================================

(define-for-syntax error-source (make-parameter #f))

(define-for-syntax (error: message stx)
  (raise-syntax-error (error-source) message (map syntax->datum stx)))

;;================================================================================
;;    rewrite-once form
;;================================================================================

(define-for-syntax (conditional? l)
  (and (pair? (syntax-e (cadr l)))
       (eq? '? (syntax-e (car (syntax-e (cadr l)))))))

(define-for-syntax (=>? l)
  (and (pair? (syntax-e (cadr l)))
       (eq? '=> (syntax-e (car (syntax-e (cadr l)))))))


;; parsing the reduction sequence
;; p __1 --> r  ==>  [(list p) r]
(define-for-syntax (parse-RS-rules stx)
  (parameterize ([error-source 'rewrite])
    (let loop ([l (syntax->list stx)])
      (cond
        [(null? l) '()]
        [(eq? (syntax-e (car l)) '-->) (error: "Missing pattern" l)]
        [(null? (cdr l)) (error: "Missing reduction rule after pattern" l)]
        [else 
         (let read-patterns ([l l] [res '()])
           (cond
             [(null? (cdr l)) (error: "Missing reduction rule" res)]
             [else 
              (case (syntax-e (car l))
                ['--> (cond
                        [(conditional? l)
                         (append (list (list (cons 'list (reverse res)) 
                                             `(=> fail)
                                             `(if ,(cadr (syntax-e (cadr l))) 
                                                  ,(caddr l) 
                                                  (fail))))
                                 (loop (cdddr l)))]
                        [(=>? l) 
                         (append (list (list (cons 'list (reverse res)) 
                                             (cadr l)
                                             (caddr l)))
                                 (loop (cdddr l)))]
                        [else (append (list (list (cons 'list (reverse res)) 
                                                  (cadr l)))
                                      (loop (cddr l)))])]
                ['-->. (error: "Terminal reduction rule could be used only inside rewrite-repeated form."
                               (list (car res) (car l) (cadr l)))]
                [else (read-patterns (cdr l) (cons (car l) res))])]))]))))

(define-for-syntax (arity lst)
  (let ([rule-arity (λ (rule)
                      (cond
                        [(member '... (map syntax-e rule)) #`(arity-at-least #,(- (length rule) 1))]
                        [else (length rule)]))])
    (map rule-arity (map cdar lst))))

;; unarity test: returns #t if all patterns are unary
(define-for-syntax (only-unary-rules? stx src)
  (parameterize ([error-source src])
    (let loop ([l (syntax->list stx)])
      (cond
        [(null? l) #t]
        [(eq? (syntax-e (car l)) '-->)
         (error: "Missing pattern" l)]
        [(null? (cdr l)) (error: "Missing reduction rule after pattern" l)]
        [(null? (cddr l)) (error: "Missing reduction rule" l)]
        [else (and (eq? (syntax-e (cadr l)) '-->)
                   (loop (cdddr l)))]))))

;: Produces a reduction system which is applied once.
(define-syntax (rewrite stx) ;: red ... -> procedure?
  (syntax-case stx ()
    [(_ rules ...) 
     (let ([RS (parse-RS-rules #'(rules ...))])
       (with-syntax ([(p ...) RS]
                     [(ar ...) (arity RS)])
         #'(procedure-rename
            (procedure-reduce-arity
             (match-lambda* 
               p ... 
               [(list any) any] 
               [any any])
             (list ar ...))
            'rewrite)))]))

;; The reductions are listable.
(define-syntax (rewrite-all stx) ;: red ... -> procedure?
  (syntax-case stx ()
    [(_ rules ...) 
     (let ([only-unary? (only-unary-rules? #'(rules ...) 'rewrite)]
           [RS (parse-RS-rules #'(rules ...))])
       (with-syntax  ([(p ...) RS]
                      [(ar ...) (arity RS)])
         (if (and #t only-unary?)
             #'(letrec 
                   ([f (match-lambda* 
                         p ... 
                         #;[(list (? formal? any)) (map f any)]
                         [(list (? list? any)) (map f any)] 
                         [(list any) any] 
                         [any any])])
                 (procedure-rename 
                  (procedure-reduce-arity 
                   f 
                   1 )
                  'rewrite-all))
             #'(letrec 
                   ([f (match-lambda* 
                         p ... 
                         #;[(list (? formal? any)) (map f any)]
                         [(list (? list? any)) (map f any)] 
                         [(list any) any] 
                         [any any])])
                 (procedure-rename 
                  (procedure-reduce-arity 
                   f 
                   (list ar ...))
                  'rewrite-all)))))]))

;;================================================================================
;; Aliases
;;================================================================================
;: Binds symbol name with reduction system so that it could be used within reduction rules.
;: symbol? rules ... -> procudure?
(define-syntax-rule (define/. name rules ...) 
  (define name (rewrite-all rules ...)))

(define-syntax-rule (/. rules ...) (rewrite-all rules ...))
