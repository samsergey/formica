#lang racket/base
;;______________________________________________________________
;;                   ______ 
;;                  (  //   _____ ____   .  __  __
;;                   ~//~ ((_)// // / / // ((_ ((_/_
;;                 (_//
;;..............................................................
;; Provides tools to work with monads.
;;==============================================================
(require "../tools.rkt"
         "../tags.rkt"
         "../types.rkt"
         racket/match
         (for-syntax racket/base racket/syntax ))

(provide (rename-out (make-monad monad)
                     (make-monad-plus monad-plus))
         >>=
         do
         collect
         define-monad
         define-monad-plus
         monad? monad-plus?
         using-monad using
         return lift lifted? bind
         mzero mplus msum 
         m-compose
         fold-m filter-m 
         map-m lift-m
         guard guardf
         Id)
;;;==============================================================
;;; General definitions
;;;==============================================================
;; monad is an abstract data type with return and bind operations
(struct monad (type return bind))
;; monad-plus is a monad with generalized operation mplus and neutral element
(struct monad-plus monad (mzero mplus))

;;;==============================================================
;;; monad constructors
;;;==============================================================
(define (make-monad #:type (type any/c) #:bind bind #:return return)
  (monad type return bind))

(define (make-monad-plus #:type (type any/c) #:bind bind #:return return #:mzero mzero #:mplus mplus)
  (monad-plus type return bind mzero mplus))

;; helper function for define-monad syntax
(define (make-monad* M name #:type (type any/c) #:bind bind #:return return)
  (M type     
     (procedure-rename return 'return) 
     (procedure-rename (λ (x f) 
                         (unless (is x type) (error name "!!"))
                         (bind x f))
                       'bind)))

(define-syntax (define-monad stx)
  (syntax-case stx ()
    [(define-monad id args ...) 
     (with-syntax ([mn (format-id #'id "monad:~a" (syntax-e #'id))])
       #`(begin
           (struct mn monad ())
           (define id (make-monad* mn 'id args ...))))]))

;; helper function for define-monad-plus syntax
(define (make-monad-plus* M name #:type (type any/c) #:bind bind #:return return #:mzero mzero #:mplus mplus)
  (M type 
     (procedure-rename return 'return)
     (procedure-rename (λ (x f) 
                         (unless (is x type) (raise-argument-error 'bind (format "monadic type ~a (monad ~a)" type name) x))
                         (bind x f)) 
                       'bind)
     (begin
       (unless (is mzero type) (raise-argument-error 'monad-plus (format "mzero, having monadic type ~a (monad ~a)" type name) mzero))
       mzero)
     (procedure-rename mplus 'mplus)))

(define-syntax (define-monad-plus stx)
  (syntax-case stx ()
    [(define-monad-plus id args ...) 
     (with-syntax ([mn (format-id #'id "monad-plus:~a" (syntax-e #'id))])
       #`(begin
           (struct mn monad-plus ())
           (define id (make-monad-plus* mn 'id args ...))))]))

;;;===============================================================================
;;; The Id monad
;;;===============================================================================
(define-monad Id
  #:type Any
  #:return id 
  #:bind   (λ(x f) (f x)))

;;;==============================================================
;;; Syntax sugar for monadic functions
;;;==============================================================
;; lifting a value into the current monad
(define-syntax return
  (syntax-id-rules ()
    ((return expr ...) ((monad-return (using-monad)) expr ...))
    (return (monad-return (using-monad)))))

;; binding in the current monad
;; Haskel: m >>= f >>= g
;; Formica: (bind m >>= f >>= g)
(define-syntax bind
  (syntax-id-rules (>>= >>)
    ((bind m >>= f) ((monad-bind (using-monad)) m f))
    ((bind m >> f) ((monad-bind (using-monad)) m (λ (_) f)))
    ((bind m ar1 f ar2 fs ...) (bind (bind m ar1 f) ar2 fs ...))
    (bind (monad-bind (using-monad)))))

;; single >>=
(define-syntax >>=
  (syntax-id-rules ()
    [>>= (raise-syntax-error '>>= "could be used only in bind form")]))

;; mzero of the current monad
(define-syntax mzero
  (syntax-id-rules ()
    (mzero (monad-plus-mzero (using-monad)))))

;; mplus of the current monad
(define-syntax mplus
  (syntax-id-rules ()
    ((mplus expr ...) ((monad-plus-mplus (using-monad)) expr ...))
    (mplus (monad-plus-mplus (using-monad)))))

;; do syntax
;; Haskel: do { p1 <- m; p2 <- f; ...;  g p1 p2}
;; Formica: (do (p1 <- m) (p2 <- f) ...  (g p1 p2))
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
                                    [expr (error "do: no matching clause for" 'p)]))]
    [(do* (p <-: m) r) (do* (p <- (return m)) r)]
    [(do* (b ...) r) (bind (b ...) >> r)]))

;; monadic generator
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
(define-syntax (m-compose stx)
  (syntax-case stx (>>=)
    [(_ f) #'(procedure-rename
              (λ x (bind (apply return x) >>= f))
              'm-composed)]
    [(_ f g) #'(procedure-rename
                (λ x (bind (apply return x) >>= g >>= f))
                'm-composed)]
    [(_ f ...) (with-syntax ([(s ...) (local-expand 
                                       #'(seq f ...) 'expression #f)])
                 #'(procedure-rename
                    (λ x (bind (apply return x) >>= s ...))
                    'm-composed))]))

(define-syntax (seq stx)
  (syntax-case stx (>>=)
    [(_ f g) #'(g >>= f)]
    [(_ g ... f)  (with-syntax ([(expanded-seq ...) (local-expand 
                                               #'(seq g ...) 'expression #f)])
                    #`(f >>= expanded-seq ...))]))

;; lifting the function
(define (lift f)
  (if (lifted? f) 
      f
      ((set-tag 'lifted (or (object-name f) 'λ)) (compose1 return f))))

(define (lifted? f) (check-tag 'lifted f))

(define-syntax (lift-m stx)
  (syntax-case stx ()
    [(lift-m f x ...) 
     (with-syntax ([(x-id ...) (generate-temporaries #'(x ...))])
       #'(do (x-id <- x) ... 
             (return (f x-id ...))))]))

;; guarding operator
(define (guard test)
  (unless (monad-plus? (using-monad))
    (error 'guard (format "~a is not of a type monad-plus!" (using-monad))))
  (if test (return 'null) mzero))

;; guarding function
(define ((guardf pred?) x)
  (bind (guard (pred? x)) >> (return x)))


;; monadic folds
(define (msum lst) (foldr mplus mzero lst))

(define fold-m
  (procedure-rename
   (match-lambda*
     [(list f a '()) (return a)]
     [(list f a (cons x xs)) (do [y <- (f a x)] 
                                 (fold-m f y xs))])
   'fold-m))

(define filter-m
  (procedure-rename
   (match-lambda*
     [(list _ '()) (return '())]
     [(list p (cons x xs)) (do [b <- (p x)]
                               [ys <- (filter-m p xs) ]
                               (return (if b (cons x ys) ys)))])
   'filter-m))

(define (map-m f ms)
  (for/fold ((res mzero)) ((x ms))
    (mplus (f x) res)))

;;;===============================================================================
;;; Managing the used monad
;;;===============================================================================
;; the monad used by default
(define using-monad (make-parameter Id))

;; locally used monad
(define-syntax-rule (using M expr ...) 
  (parameterize ([using-monad M]) expr ...))
