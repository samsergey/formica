#lang racket/base
;;______________________________________________________________
;;                   ______ 
;;                  (  //   _____ ____   .  __  __
;;                   ~//~ ((_)// // / / // ((_ ((_/_
;;                 (_//
;;..............................................................
;; Provides basic tools to work with monads.
;;==============================================================
(require "../../tags.rkt"
         (only-in "../../types.rkt" 
                  check-result 
                  check-argument
                  check-type)
         racket/match
         racket/contract
         (for-syntax racket/base racket/syntax ))

(provide (rename-out (make-monad monad))
         ; forms
         >>= >> <- <-: <<- <<-:
         do
         collect
         define-monad
         using
         check-result
         ; functional forms
         return
         bind
         mzero
         mplus
         failure
         lift/m
         compose/m
         ;functions
         (contract-out 
          (monad? predicate/c)
          (monad-zero? predicate/c)
          (monad-plus? predicate/c)
          (using-monad (parameter/c monad?))
          (lift (-> procedure? procedure?))
          (fold/m (-> (-> any/c any/c any/c) any/c list? any/c))
          (filter/m (-> (-> any/c any/c) list? any/c))
          (map/m (-> (-> any/c any/c) list? any/c))
          (seq/m (-> list? any/c))
          (sum/m (-> list? any/c))
          (guard (-> any/c any/c))
          (guardf (-> (-> any/c any/c) (-> any/c any/c)))
          (Id monad?)))
;;;==============================================================
;;; General definitions
;;;==============================================================
;; monad is an abstract data type
(struct monad (type return bind failure mzero mplus))

(define (raise-match-error x)
  (error "do: no matching clause for" x))

(define (monad-zero? v)
  (and (monad? v) (not (eq? 'undefined (monad-mzero v)))))

(define (monad-plus? v)
  (and (monad-zero? v) (not (eq? 'undefined (monad-mplus v)))))
;;;==============================================================
;;; monad constructors
;;;==============================================================
;; named monad constructor
(define (make-named-monad name
                          #:type (type #f)
                          #:bind bind 
                          #:return return
                          #:mzero (mzero 'undefined)
                          #:mplus (mplus 'undefined)
                          #:failure (failure raise-match-error))
  (define return*
    (if type 
        (procedure-reduce-arity
         (λ x (let ([res (apply return x)])
                (if (equal? res (return ⊤))
                    (return ⊤)
                    (check-result 'return type res))))
         (procedure-arity return))
        return))
  
  (define bind*
    (if type 
        (λ (m f)
          (unless (equal? m (return ⊤)) (check-argument 'bind type m))
          (check-result 'bind type (bind m f)))
        bind))
  
  (name 
   type
   (procedure-rename return* 'return)
   (procedure-rename bind* 'bind)
   failure
   mzero
   (if (procedure? mplus) 
       (procedure-rename mplus 'mplus) 
       mplus)))

;; anonymous monad constructor
(define (make-monad #:type (type #f)
                    #:bind bind 
                    #:return return
                    #:mzero (mzero 'undefined)
                    #:mplus (mplus 'undefined)
                    #:failure (failure raise-match-error))
  (make-named-monad monad 
                    #:type type
                    #:bind bind 
                    #:return return
                    #:mzero mzero
                    #:mplus mplus
                    #:failure failure))


(define-syntax (define-monad stx)
  (syntax-case stx ()
    [(define-monad id args ...) 
     (with-syntax ([mn (format-id #'id "monad:~a" (syntax-e #'id))])
       #`(begin
           (struct mn monad ())
           (define id (make-named-monad mn args ...))))]))

;;;===============================================================================
;;; The Id monad
;;;===============================================================================
(define-monad Id
  #:return (λ (x) x) 
  #:bind   (λ (x f) (f x)))

;;;===============================================================================
;;; Managing the used monad
;;;===============================================================================
;; the monad used by default
(define using-monad (make-parameter Id))

;; locally used monad
(define-syntax-rule (using M expr ...) 
  (parameterize ([using-monad M]) expr ...))

(define-syntax-rule (when-monad-zero expr id)
  (if (monad-zero? (using-monad))
      expr
      (error id (format "~a is not of a type <monad-zero?>" (using-monad)))))

(define-syntax-rule (when-monad-plus expr id)
  (if (monad-plus? (using-monad))
      expr
      (error id (format "~a is not of a type <monad-plus?>" (using-monad)))))
;;;==============================================================
;;; Syntax sugar for monadic functions
;;; functions return, bind, mzero and mplus
;;; are syntax forms in order to track the current monad
;;;==============================================================
;; return in the current monad
(define-syntax return
  (syntax-id-rules ()
    ((return expr ...) ((monad-return (using-monad)) expr ...))
    (return (monad-return (using-monad)))))

;; binding in the current monad
;; Haskel: m >>= f >>= g
;; Formica: (bind m >>= f >>= g)
(define-syntax bind
  (syntax-id-rules (>>= >>)
    ((bind m ar1 f ar2 fs ...) (bind (bind m ar1 f) ar2 fs ...))
    ((bind m >>= f) ((monad-bind (using-monad)) m f))
    ((bind m >> f) ((monad-bind (using-monad)) m (λ (_) f)))
    (bind (monad-bind (using-monad)))))

;; single >>=
(define-syntax >>=
  (syntax-id-rules ()
    [>>= (raise-syntax-error '>>= "could be used only in bind form")]))

;; single >>=
(define-syntax >>
  (syntax-id-rules ()
    [>> (raise-syntax-error '>> "could be used only in bind form")]))


;; mzero of the current monad
(define-syntax mzero
  (syntax-id-rules ()
    [mzero (when-monad-zero (monad-mzero (using-monad)) 'mzero)]))

;; mplus of the current monad
(define-syntax mplus
  (syntax-id-rules ()
    [(mplus expr ...) (when-monad-plus ((monad-mplus (using-monad)) expr ...) 'mplus)]
    [mplus (when-monad-plus (monad-mplus (using-monad)) 'mplus)]))

;; mplus of the current monad
(define-syntax failure
  (syntax-id-rules ()
    [(failure expr) ((monad-failure (using-monad)) expr)]
    [failure (monad-failure (using-monad))]))

;; do syntax
;; Haskel: do { p1 <- m; p2 <- f; ...;  expr}
;; Formica: (do (p1 <- m) (p2 <- f) ...  expr)
(define-syntax (do stx)
  (syntax-case stx ()
    [(do b r) #'(do* b r)]
    [(do b bs ... r) 
     (with-syntax ([expanded-do (local-expand 
                                 #'(do bs ... r) 'expression #f)])
       #`(do* b expanded-do))]))

(define-syntax do* 
  (syntax-rules (<- <-: <<- <<-:)
    [(do* ((p ...) <<- m) r) (do (p <- m) ... r)]
    [(do* ((p ...) <<-: m) r) (do (p <-: m) ... r)]
    [(do* (p <- m) r) (bind m >>= (match-lambda
                                    [p r] 
                                    [expr (failure expr)]))]
    [(do* (p <-: m) r) (do* (p <- (return m)) r)]
    [(do* (b ...) r) (bind (b ...) >> r)]))

;; single arrows
(define-syntax <-
  (syntax-id-rules ()
    [<- (raise-syntax-error '<- "could be used only in do or collect form")]))
(define-syntax <-:
  (syntax-id-rules ()
    [<-: (raise-syntax-error '<-: "could be used only in do or collect form")]))
(define-syntax <<-
  (syntax-id-rules ()
    [<<- (raise-syntax-error '<<- "could be used only in do or collect form")]))
(define-syntax <<-:
  (syntax-id-rules ()
    [<<-: (raise-syntax-error '<<-: "could be used only in do or collect form")]))


;; monadic generator
;; Haskel: [expr | p1 <- m; p2 <- f]
;; Formica: (collect expr [p1 <- m] [p2 <- f])
(define-syntax-rule (collect res x ...)
  (collect* x ... (return res)))

(define-syntax (collect* stx)
  (syntax-case stx ()
    [(_ b r) #'(collect** b r)]
    [(_ b bs ... r) 
     (with-syntax ([expanded-do (local-expand 
                                 #'(collect* bs ... r) 'expression #f)])
       #`(collect** b expanded-do))]))

(define-syntax collect** 
  (syntax-rules (<- <-: <<- <<-:)
    [(_ ((p ...) <<- m) r) (do (p <- m) ... r)]
    [(_ ((p ...) <<-: m) r) (do (p <-: m) ... r)]
    [(_ (p <-: m) r) (do (p <-: m) r)]
    [(_ (p <- m) r) (do (p <- m) r)]
    [(_ expr r) (bind (guard expr) >> r)]))

;;;==============================================================
;;; Monadic functions
;;;==============================================================
;; monadic composition 
(define-syntax (compose/m stx)
  (syntax-case stx ()
    [(_ f) #'(procedure-rename
              (procedure-reduce-arity
               (λ x (bind (apply return x) >>= f))
               (procedure-arity f))
              'composed/m)]
    [(_ f g) #'(procedure-rename
                (procedure-reduce-arity
                (λ x (bind (apply return x) >>= g >>= f))
                (procedure-arity g))
                'composed/m)]
    [(_ f ... g) (with-syntax ([(s ...) (local-expand 
                                       #'(seq f ... g) 'expression #f)])
                 #'(procedure-rename
                    (procedure-reduce-arity
                     (λ x (bind (apply return x) >>= s ...))
                     (procedure-arity g))
                    'composed/m))]))

(define-syntax (seq stx)
  (syntax-case stx (>>=)
    [(_ f g) #'(g >>= f)]
    [(_ g ... f)  (with-syntax ([(expanded-seq ...) (local-expand 
                                                     #'(seq g ...) 'expression #f)])
                    #`(f >>= expanded-seq ...))]))

;; lifting the function
(define (lift f) (compose1 return f))

(define-syntax (lift/m stx)
  (syntax-case stx ()
    [(_ f x xs ...) 
     (with-syntax ([(x-id x-ids ...) (generate-temporaries #'(x xs ...))])
       #'(do (x-id <- x)
             (x-ids <- xs) ... 
             (return (f x-id x-ids ...))))]))

;; guarding operator
;; The ⊤ symbol represents an instance that belongs to any type
;; It is used for avoid type checking within binding.
(define ⊤ '⊤)
(define (guard test)
  (when-monad-zero (if test (return ⊤) mzero) 'guard))

;; guarding function
(define (guardf pred?)
  (when-monad-zero 
   (λ (x) (bind (guard (pred? x)) >> (return x))) 
   'guardf))


;; monadic fold
(define fold/m
  (procedure-rename
   (match-lambda**
    [(f a '()) (return a)]
    [(f a (cons x xs)) (do [y <- (f x a)] 
                           (fold/m f y xs))])
   'fold/m))

;; monadic filtering
(define filter/m
  (procedure-rename
   (match-lambda**
    [(_ '()) (return '())]
    [(p (cons x xs)) (do [b <- (p x)]
                         [ys <- (filter/m p xs) ]
                         (return (if b (cons x ys) ys)))])
   'filter/m))

;; monadic sequencing
(define (seq/m lst) 
  (foldr (λ (x y) (lift/m cons x y)) (return '()) lst))

;; monadic map
(define (map/m f lst) (seq/m (map f lst)))

;; monadic sum
(define (sum/m lst) 
  (when-monad-plus (foldr mplus mzero lst) 'sum/m))
