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
         (for-syntax racket/base)
         "../tools/tags.rkt")

(provide 
 ::
 define-type
 (rename-out (.->. ->))
 Any Bool Num Real Int Nat Index Str Sym Fun Fun/c Type
 ∩ ∪ \\ complement/c
 is check-result check-argument check-type
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
        (foldl (λ(s res) 
                 (let ([l (syntax->list s)])
                   (cond
                     ; go deep into lists
                     [l (foldl include res (free-symbols s))]
                     ; special symbols
                     [(or (eq? (syntax-e s) '?)
                          (eq? (syntax-e s) '..)) res]
                     ; unknown identifiers
                     [(and (identifier? s)
                           (not (identifier-binding s))) (include s res)]
                     ; unknown identifiers
                     [else res])))
               '()
               lst)
        stx)))

(define-syntax-rule (:: name c body)
  (begin
    body
    (set! name (::* c name))))


(define-syntax (::* stx)
  (syntax-case stx ()
    [(::* c body) 
     (let ([f (free-symbols #'c)])
         (if (null? f)
             ; no free symbols
             #`(contract #,(parse-infix-contract #'c)
                         body
                         5 6)
             ; polymorphic types
             (with-syntax ([(free ...) f])
               #`(contract (parametric->/c (free ...) #,(parse-infix-contract #'c))
                           body
                           5 6))))]))

;;;=================================================================
;;; type definitions
;;;=================================================================
(define-syntax define-type
  (syntax-rules (_)
    ; parameterized type
    [(_ (name A ...) expr ...) 
     (define (name A ...)
       (flat-named-contract 
        (cons 'name (map (λ (x) (or (object-name x) x)) (list A ...)))
        (λ (x) (or ((flat-contract expr) x) ...))))]
    ; primitive type or type product
    [(_ name expr) 
     (define name 
       (procedure-rename 
        (flat-named-contract 'name expr) 
        'name))]
    ; type sum
    [(_ name expr ...) 
     (define name 
       (procedure-rename
        (flat-named-contract 
         'name
         (flat-rec-contract name (or/c expr ...))) 
        'name))]))


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
   "Signature violation!\n "
   message
   (format "\n signature:  ~a :: ~a" 
           (blame-value blame) 
           (translate-contract (blame-contract blame)))))


(current-blame-format show-blame-error)


;;;=================================================================
;;; aliaces
;;;=================================================================
(define complement/c
  (procedure-rename
   (case-lambda
     [(A) (not/c A)]
     [(A B) (and/c A (not/c B))]
     [(A B . C) (apply complement/c (complement/c A B) C)])
   'complement/c))



(define ∩ and/c)
(define ∪ or/c)
(define \\ complement/c)
(define-type Any any/c)
(define-type Bool boolean?)
(define-type Num number?)
(define-type Real real?)
(define-type Int integer?)
(define-type Nat natural-number/c +inf.0)
(define-type Index (and/c integer? (>/c 0) +inf.0))
(define-type Str string?)
(define-type Sym symbol?)
(define-type Fun procedure?)
(define-type Type contract?)
(define-type (Fun/c name)
  (and/c procedure?
         (λ(f)(eq? (object-name f) name))))

(define-syntax list:
  (syntax-id-rules (..)
    [(_ c ..) ((procedure-rename listof 'list:) c)]
    [(_ c ...) ((procedure-rename list/c 'list:) c ...)]
    [list: (procedure-rename list/c 'list:)]))

(define cons: (procedure-rename cons/c 'cons:))