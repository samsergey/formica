#lang racket/base
;;______________________________________________________________
;;                   ______ 
;;                  (  //   _____ ____   .  __  __
;;                   ~//~ ((_)// // / / // ((_ ((_/_
;;                 (_//
;;..............................................................
;; Provides rewrite-repeated form.
;;==============================================================
(require racket/match
         (for-syntax racket/base)
         (only-in "../tools/functionals.rkt" fixed-point))


(provide rewrite-repeated
         rewrite-all-repeated //.
         define//.
         (for-syntax parse-multiary-rules
                     rules-arity))

;;====================================================
;; error messages
;;====================================================

(define-for-syntax error-source (make-parameter #f))

(define-for-syntax (error: message stx)
  (raise-syntax-error (error-source) message (map syntax->datum stx)))

;;================================================================================
;;   different expressions
;;================================================================================
(define-for-syntax (conditional? l)
  (and (pair? (syntax-e (cadr l)))
       (eq? '? (syntax-e (car (syntax-e (cadr l)))))))

(define-for-syntax (=>? l)
  (and (pair? (syntax-e (cadr l)))
       (eq? '=> (syntax-e (car (syntax-e (cadr l)))))))

(define-for-syntax (conditional*? l)
  (and (pair? (syntax-e (caddr l)))
       (eq? '? (syntax-e (car (syntax-e (caddr l)))))))

(define-for-syntax (=>*? l)
  (and (pair? (syntax-e (caddr l)))
       (eq? '=> (syntax-e (car (syntax-e (caddr l)))))))

(define-for-syntax (using-terminal-rules? stx)
  (memq '-->. (map syntax-e (syntax->list stx))))

(define-for-syntax (only-terminal-rules? stx)
  (not (memq '--> (map syntax-e (syntax->list stx)))))

;; unarity test: returns #t if all patterns are unary
(define-for-syntax (only-unary-rules? stx)
  (parameterize ([error-source 'rewrite])
    (let loop ([l (syntax->list stx)])
      (cond
        [(null? l) #t]
        [(or (eq? (syntax-e (car l)) '-->)
             (eq? (syntax-e (car l)) '-->.))
         (error: "Missing pattern" l)]
        [(null? (cdr l)) (error: "Missing reduction rule after pattern" l)]
        [(null? (cddr l)) (error: "Missing reduction rule" l)]
        [else (and (or (eq? (syntax-e (cadr l)) '-->)
                       (eq? (syntax-e (cadr l)) '-->.))
                   (loop (cdddr l)))]))))

;; parsing the rewrite reduction sequence
;; p --> r  ==>  [p r]
;; p -->. r  ==>  [p (stop r)]
(define-for-syntax (parse-unary-rules stop-id stx)
  (parameterize ([error-source 'rewrite])
    (let loop ([l (syntax->list stx)])
      (cond
        [(null? l) '()]
        [(null? (cdr l)) (error: "Missing reduction rule after pattern" l)]
        [(null? (cddr l)) (error: "Missing reduction rule" l)]
        [else (case (syntax-e (cadr l))
                ['--> (cond
                        [(conditional*? l)
                         (append (list (list (car l)
                                             `(=> fail)
                                             `(if ,(cadr (syntax-e (caddr l)))
                                                  ,(cadddr l)
                                                  (fail)))) 
                                 (loop (cddddr l)))]
                        [(=>*? l) 
                         (append (list (list (car l) 
                                             (caddr l) 
                                             (cadddr l))) 
                                 (loop (cddddr l)))]
                        [else (append (list (list (car l) (caddr l))) 
                                      (loop (cdddr l)))])]
                ['-->. (cond
                         [(conditional*? l)
                          (append (list (list (car l)
                                              `(=> fail)
                                              `(if ,(cadr (syntax-e (caddr l)))
                                                   ,(list stop-id (cadddr l))
                                                   (fail)))) 
                                  (loop (cddddr l)))]
                         [(=>*? l) 
                          (append (list (list (car l) 
                                              (caddr l) 
                                              (list stop-id (cadddr l)))) 
                                  (loop (cddddr l)))]
                         [else (append (list (list (car l) 
                                                   (list stop-id (caddr l)))) 
                                       (loop (cdddr l)))]) ]
                [else (error:  "--> or -->. expected" 
                               (list (car l) (cadr l) (caddr l)))])]))))

;; parsing the rewrite reduction sequence
;; p __1 --> r  ==>  [(list p) r]
;; p __1 -->. r  ==>  [(list p) (stop r)]
(define-for-syntax (parse-multiary-rules stop-id stx)
  (parameterize ([error-source 'rewrite])
    (let loop ([l (syntax->list stx)])
      (cond
        [(null? l) '()]
        [(null? (cdr l)) (error: "Missing reduction rule after pattern" l)]
        [else (let read-patterns ([l l] [res '()])
                (if (null? (cdr l)) 
                    (error: "Missing reduction rule" res)
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
                      ['-->. (cond
                               [(conditional? l)
                                (append (list (list (cons 'list (reverse res)) 
                                                    `(=> fail)
                                                    `(if ,(cadr (syntax-e (cadr l))) 
                                                         ,(list stop-id (caddr l)) 
                                                         (fail))))
                                        (loop (cdddr l)))]
                               [(=>? l) 
                                (append (list (list (cons 'list (reverse res)) 
                                                    (cadr l)
                                                    (list stop-id (caddr l))))
                                        (loop (cdddr l)))]
                               [else (append (list (list (cons 'list (reverse res)) 
                                                         (list stop-id (cadr l)))) 
                                             (loop (cddr l)))]) ]
                      [else (read-patterns (cdr l) (cons (car l) res))])))]))))

;; returns the arity of rewriting system
(define-for-syntax (rules-arity lst)
  (let ([rule-arity (λ (rule)
                      (cond
                        [(member '... (map syntax-e rule)) #`(arity-at-least #,(- (length rule) 1))]
                        [else (length rule)]))])
    (map rule-arity (map cdar lst))))

;: Produces a reduction system which is applied repeatedly
;; utill reaching the normal form of given input
;; The reductions are listable
;: red ... ->  procedure?
(define-syntax (rewrite-repeated stx) 
  (syntax-case stx ()
    [(_ rules ...) 
     (let ([stop-id (car (generate-temporaries #'(stop)))]
           [only-unary? (only-unary-rules? #'(rules ...))])
       (if only-unary?
           (with-syntax ([(p ...) (parse-unary-rules stop-id #'(rules ...))])
             (cond
               [(using-terminal-rules? #'(rules ...))
                #`(procedure-rename
                   (λ (x)
                     (let/cc #,stop-id
                       ((fixed-point 
                         (match-lambda p ... [any any])) x)))
                   'rewrite-repeated)]
               
               [(only-terminal-rules? #'(rules ...))
                #'(procedure-rename
                   (match-lambda p ... [any any])
                   'rewrite)]
               [else
                #`(procedure-rename
                   (fixed-point
                    (match-lambda p ... [any any]))                   
                   'rewrite-repeated)]))
           (let ([RS (parse-multiary-rules stop-id #'(rules ...))])
             (with-syntax ([(p ...) RS]
                           [(ar ...) (rules-arity RS)])
               (cond
                 [(using-terminal-rules? #'(rules ...))
                  #`(procedure-rename
                     (procedure-reduce-arity 
                      (λ x
                        (let/cc #,stop-id
                          (apply (fixed-point
                                  (match-lambda* 
                                    p ... 
                                    [(list any) any] 
                                    [any any])) x)))
                      (list ar ...))
                     'rewrite-repeated)]
                 
                 [(only-terminal-rules? #'(rules ...))
                  #'(procedure-rename
                     (procedure-reduce-arity 
                      (match-lambda* 
                        p ... 
                        [(list any) any] 
                        [any any])
                      (list ar ...))
                     'rewrite)]
                 
                 [else
                  #`(procedure-rename
                     (procedure-reduce-arity 
                      (fixed-point
                       (match-lambda* 
                         p ...                  
                         [(list any) any] 
                         [any any]))
                      (list ar ...))
                     'rewrite-repeated)])))))]))

(define-syntax (rewrite-all-repeated stx) 
  (syntax-case stx ()
    [(_ rules ...) 
     (let ([stop-id (car (generate-temporaries #'(stop)))]
           [only-unary? (only-unary-rules? #'(rules ...))])
       (if only-unary?
           (with-syntax ([(p ...) (parse-unary-rules stop-id #'(rules ...))])
             (cond
               [(using-terminal-rules? #'(rules ...)) 
                #`(letrec ([f (λ (x)
                                (let/cc #,stop-id
                                  ((fixed-point 
                                    (match-lambda 
                                      p ... 
                                      [(? list? any) (#,stop-id (map f any))]
                                      [any any])) x)))])
                    (procedure-rename f 'rewrite-all-repeated))]
               
               [(only-terminal-rules? #'(rules ...))
                #'(letrec ([f (match-lambda 
                                p ...
                                [(? list? any) (map f any)]
                                [any any])])
                    (procedure-rename f 'rewrite-all))]
               [else
                #`(letrec ([f (match-lambda 
                                p ...
                                [(? list? any) (map f any)]
                                [any any])])
                    (procedure-rename (fixed-point f) 'rewrite-all-repeated))]))
           (let ([RS (parse-multiary-rules stop-id #'(rules ...))])
             (with-syntax ([(p ...) RS]
                           [(ar ...) (rules-arity RS)])
               (cond
                 [(using-terminal-rules? #'(rules ...))
                  #`(letrec ([f (λ x
                                  (let/cc #,stop-id
                                    (apply (fixed-point
                                            (match-lambda* 
                                              p ...
                                              [(list (? list? any)) (#,stop-id (map f any))]
                                              [(list any) any] 
                                              [any any])) x)))])
                      (procedure-rename 
                       (procedure-reduce-arity 
                        f
                        (list ar ...))
                       'rewrite-all-repeated))]
                 
                 [(only-terminal-rules? #'(rules ...))
                  #'(letrec ([f (match-lambda* 
                                  p ...
                                  [(list (? list? any)) (map f any)]
                                  [(list any) any] 
                                  [any any])])
                      (procedure-rename 
                       (procedure-reduce-arity 
                        f
                        (list ar ...))
                       'rewrite-all))]
                 
                 [else
                  #`(letrec ([f (match-lambda* 
                                  p ... 
                                  [(list (? list? any)) (map f any)]
                                  [(list any) any] 
                                  [any any])])
                      (procedure-rename 
                       (procedure-reduce-arity 
                        (fixed-point f)
                        (list ar ...))
                       'rewrite-all-repeated))])))))]))


;;================================================================================
;; Aliases
;;================================================================================
;: Binds symbol name with reduction system so that it could be used withen reduction rules.
;: symbol? rules ... -> procudure?
(define-syntax-rule (define//. name rules ...) 
  (define name (rewrite-all-repeated rules ...)))

(define-syntax-rule (//. rules ...) (rewrite-all-repeated rules ...))