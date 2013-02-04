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
         "infix-arrow.rkt"
         "type-checking.rkt"
         "../tools/tags.rkt"
         "../formal/formal.rkt")

(provide 
 ::
 (rename-out (.->. ->))
 define-type
 Any Bool Num Real Int Nat Index Str Sym Fun Fun/c Type
 ∩ ∪ \\ complement/c
 is check-result check-argument check-type
 list: cons:
 (except-out (all-from-out racket/contract)
             ->
             list/c
             cons/c))

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

(define-syntax (:: stx)
  (syntax-case stx ()
    [(:: name c body) 
     (let ([f (free-symbols #'c)])
       (if (null? f)
           ; no free symbols
           #`(with-contract name ((name #,(parse-infix-contract #'c))) body)
           ; polymorphic types
           (with-syntax ([(free ...) f])
             #`(with-contract name
                              ((name (parametric->/c (free ...) #,(parse-infix-contract #'c))))
                              body))))]))

;;;=================================================================
;;; type definitions
;;;=================================================================
(define-syntax (define-type stx)
  (syntax-case stx (? ..)    
    ; abstract type with contract
    [(_ (id c ...)) 
     #'(define-formal (id #:contract (c ...)))]
    ; abstract type
    [(_ name) #'(define-formal name)]
    ; parameterized type
    [(_ (name A ...) expr ...) 
     #'(define (name A ...)
         (coerce-flat-contract 
          (cons 'name (map (λ (x) (or (object-name x) x)) (list A ...)))
          (λ (x) (or (is x (flat-contract expr)) ...))))]
    ; primitive type or type product
    [(_ name expr) 
     #'(define name 
         (procedure-rename 
          (coerce-flat-contract 'name expr) 
          'name))]
    ; type sum
    [(_ name expr ...) 
     #'(define name 
         (procedure-rename
          (coerce-flat-contract 
           'name
           (flat-rec-contract name (or/c expr ...))) 
          'name))]))


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
;;; list:
;;;=================================================================
(define-syntax list:
  (syntax-id-rules (.. ?)
    [(_ c ... (? x ...)) (opt-list: c ... (? x ...))]
    [(_ c ..) ((procedure-rename listof 'list:) c)]
    [(_ c ... x ..) (rename (foldr cons/c ((procedure-rename listof 'list:) x) (list c ...)) '(list: c ... x ..))]
    [(_ c ...) ((procedure-rename list/c 'list:) c ...)]
    [list: (procedure-rename list/c 'list:)]))


(define-for-syntax (add-optional s xlist)
  (reverse 
   (for/fold ([res (list s)]) ([x (in-list xlist)])
     (cons (append (car res) (list x)) res))))

(define-syntax-rule (rename expr name)
  (procedure-rename expr (string->symbol (format "~a" name))))

(define-syntax (opt-list: stx)
  (syntax-case stx (?)
    [(_ c ... (? x ...)) 
     (with-syntax ([((oc ...) ...) 
                    (add-optional (syntax->list #'(c ...))
                                  (syntax->list #'(x ...)))])
       #'(rename (or/c ((procedure-rename list/c 'list:) oc ...) ...) '(list: c ... (? x ...))))]))

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
(define-type Index (and/c integer? (>/c 0)) +inf.0)
(define-type Str string?)
(define-type Sym symbol?)
(define-type Fun procedure?)
(define-type Type contract?)
(define-type (Fun/c name)
  (and/c procedure?
         (λ(f)(eq? (object-name f) name))))

(define cons: (procedure-rename cons/c 'cons:))
