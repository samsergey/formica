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
         racket/contract/parametric
         racket/path
         racket/match
         (for-syntax racket/base))

(provide 
 ::
 define-type
 (rename-out (.->. ->))
 Any Bool Num Real Int Nat Index Str Sym Fun Fun/c
 ∩ ∪
 is
 list: cons:
 (except-out (all-from-out racket/contract)
             ->
             list/c
             cons/c))

;;;=================================================================
;;; helper functions and forms
;;;=================================================================
(define-for-syntax (parse-infix-contract stx) 
  (syntax-case stx (.->.)
    [(x ... .->. y) #`(.->. #,@(parse-infix-contract #'(x ...)) #,(parse-infix-contract #'y))]
    [(x y ...) #`(#,(parse-infix-contract #'x) #,@(parse-infix-contract #'(y ...)))]
    [x #'x]))

;; infix arrow which will replace the usual arrow in contracts
(define-syntax .->.
  (syntax-id-rules (.. ?)
    [(.->. v ... (? x ...) y .. res) (->* (v ...) (x ...) #:rest (listof y) res)]
    [(.->. v ... (? x ...) res) (->* (v ...) (x ...) res)]
    [(.->. v ... x .. res) (->* (v ...) #:rest (listof x) res)]
    [(.->. v ... res) (-> v ... res)]
    [.->. (raise-syntax-error '-> "could be used only in contract!")]))


;;;=================================================================
;;; interpretation of free symbols in contracts
;;;=================================================================
(define-for-syntax (include x lst)
  (cond
    [(null? lst) (list x)]
    [(eq? (syntax-e x) 
          (syntax-e (car lst))) lst]
    [else (cons (car lst) (include x (cdr lst)))]))

(define-for-syntax (free-symbols stx)
  (let* ([lst (syntax->list stx)])
    (if lst
        (foldl (λ(s res) (let ([l (syntax->list s)])
                           (cond
                             [l (foldl include res (free-symbols s))]
                             [(and (identifier? s)
                                   (not (eq? (syntax-e s) '?))
                                   (not (eq? (syntax-e s) '..))
                                   (eq? #f (identifier-binding s))) (include s res)]
                             [else res])))
               '()
               lst)
        stx)))

(define-syntax (:: stx)
  (syntax-case stx ()
    [(:: name contract body) 
     (let ([f (free-symbols #'contract)])
       (if (null? f)
           #`(with-contract name 
                            ((name #,(parse-infix-contract #'contract)))
                            body)
           (with-syntax ([(free ...) f])
             #`(with-contract name 
                              ((name (parametric->/c 
                                      (free ...)
                                      #,(parse-infix-contract #'contract)))) 
                             body))))]))


;;;=================================================================
;;; type definitions
;;;=================================================================
(define-syntax define-type
  (syntax-rules (_)
    [(_ (name A ...) expr ...) (define (name A ...)
                                 (flat-named-contract 
                                  (cons 'name (map object-name (list A ...)))
                                  (λ (x) (or ((flat-contract expr) x) ...))))]
    [(_ name expr) (define name (procedure-rename (flat-named-contract 'name expr) 'name))]
    [(_ name expr ...) (define name 
                         (flat-named-contract 
                          'name
                          (flat-rec-contract name (or/c expr ...))))]))


;;;=================================================================
;;; Type checking
;;;=================================================================
(define-syntax (is stx)
  (syntax-case stx ()
    [(_ x type) (with-syntax ([c (parse-infix-contract #'type)])
                  #'(cond
                      [(contract? c) (with-handlers ([exn:fail:contract? (lambda (exn) #f)])
                                       (contract-first-order-passes? c x))]
                      [else (raise-type-error 'is "predicate" 1 x type)]))]))

;;;=================================================================
;;; Blaming text
;;;=================================================================
(define translate-contract
  (match-lambda
    [`(->* (,v ...) #:rest (listof ,x) ,res) 
     (let ([v (map translate-contract v)]
           [x (translate-contract x)]
           [res (translate-contract res)])
       `(,@v ,x .. -> ,res))]
    [`(->* (,v ...) (,w ...) ,res) 
     (let ([v (map translate-contract v)]
           [w (translate-contract w)]
           [res (translate-contract res)])
       `(,@v ,w -> ,res))]
    [`(->* (,v ...) (,w ...) #:rest (listof ,x) ,res) 
     (let ([v (map translate-contract v)]
           [w (map translate-contract w)]
           [x (translate-contract x)]
           [res (translate-contract res)])
       `(,@v ,w ,x .. -> ,res))]
    [`(-> ,v ...) (let* ([v (reverse v)]
                         [v (map translate-contract v)])
                    (reverse `(,(car v) -> ,@(cdr v))))]
    [x x]))

(define translate-party
  (match-lambda
    [`(region ,name) (format "function ~a" name)]
    [(? path? x) (file-name-from-path x)]
    [x x]))

(define (show-blame-error blame value message)
  (string-append
   (format "\n  Signature violation:  ~a \n" 
           message)
   (format "  guilty party:  ~a\n  innocent party:  ~a\n  signature:  ~a : ~a" 
           (translate-party (blame-positive blame))
           (translate-party (blame-negative blame))
           (blame-value blame) 
           (translate-contract (blame-contract blame)))))


(current-blame-format show-blame-error)


;;;=================================================================
;;; aliaces
;;;=================================================================

(define ∩ and/c)
(define ∪ or/c)
(define-type Any any/c)
(define-type Bool boolean?)
(define-type Num number?)
(define-type Real real?)
(define-type Int integer?)
(define-type Nat natural-number/c)
(define-type Index (and/c integer? (>/c 0)))
(define-type Str string?)
(define-type Sym symbol?)
(define-type Fun procedure?)
(define-type (Fun/c name)
  (and/c procedure?
         (λ(f)(eq? (object-name f) name))))

(define-syntax list:
  (syntax-id-rules (..)
    [(_ c ..) (listof c)]
    [(_ c ...) (list/c c ...)]
    [list: list/c]))

(define cons: cons/c)