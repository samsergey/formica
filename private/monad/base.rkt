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
         racket/match
         racket/contract
         (for-syntax racket/base racket/syntax ))

(provide (rename-out (make-monad monad)
                     (make-monad-plus monad-plus))
         ; forms
         >>=
         do
         collect
         define-monad
         define-monad-plus
         using
         ; functional forms
         return
         bind
         mzero
         mplus
         lift/m
         compose/m
         ;functions
         lift
         (contract-out 
          (monad? predicate/c)
          (monad-plus? predicate/c)
          (using-monad (parameter/c monad?))
          ;(lift (-> procedure? lifted?))
          ;(lifted? predicate/c)
          (fold/m (-> (-> any/c any/c any/c) any/c list? any/c))
          (filter/m (-> (-> any/c any/c) list? any/c))
          (map/m (-> (-> any/c any/c) list? any/c))
          (guard (-> any/c any/c))
          (guardf (-> (-> any/c any/c) (-> any/c any/c)))
          (Id monad?)))
;;;==============================================================
;;; General definitions
;;;==============================================================
;; monad is an abstract data type with return and bind operations
(struct monad (return bind))
;; monad-plus is a monad with generalized operation mplus and neutral element
(struct monad-plus monad (mzero mplus))

;;;==============================================================
;;; monad constructors
;;;==============================================================
(define (make-monad #:bind bind 
                    #:return return)
  (monad 
   (procedure-rename return 'return)
   (procedure-rename bind 'bind)))

(define (make-monad-plus #:bind bind
                         #:return return 
                         #:mzero mzero 
                         #:mplus mplus)
  (monad-plus 
   (procedure-rename return 'return)
   (procedure-rename bind 'bind)
   mzero
   (procedure-rename mplus 'mplus)))

;; named monad constructor
(define (make-named-monad name 
                           #:bind bind 
                           #:return return)
  (name (procedure-rename return 'return) 
        (procedure-rename bind 'bind)))

(define-syntax (define-monad stx)
  (syntax-case stx ()
    [(define-monad id args ...) 
     (with-syntax ([mn (format-id #'id "monad:~a" (syntax-e #'id))])
       #`(begin
           (struct mn monad ())
           (define id (make-named-monad mn args ...))))]))

;; named monad-plus constructor
(define (make-named-monad-plus* name 
                                #:bind bind 
                                #:return return 
                                #:mzero mzero 
                                #:mplus mplus)
  (name (procedure-rename return 'return)
        (procedure-rename bind 'bind)
        mzero
        (procedure-rename mplus 'mplus)))

(define-syntax (define-monad-plus stx)
  (syntax-case stx ()
    [(define-monad-plus id args ...) 
     (with-syntax ([mn (format-id #'id "monad-plus:~a" (syntax-e #'id))])
       #`(begin
           (struct mn monad-plus ())
           (define id (make-named-monad-plus* mn args ...))))]))

;;;===============================================================================
;;; The Id monad
;;;===============================================================================
(define-monad Id
  #:return (λ (x) x) 
  #:bind   (λ(x f) (f x)))

;;;===============================================================================
;;; Managing the used monad
;;;===============================================================================
;; the monad used by default
(define using-monad (make-parameter Id))

;; locally used monad
(define-syntax-rule (using M expr ...) 
  (parameterize ([using-monad M]) expr ...))

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
                                    [expr (error "do: no matching clause for" 'p)]))]
    [(do* (p <-: m) r) (do* (p <- (return m)) r)]
    [(do* (b ...) r) (bind (b ...) >> r)]))

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
  (syntax-case stx (>>=)
    [(_ f) #'(procedure-rename
              (λ x (bind (apply return x) >>= f))
              'composed/m)]
    [(_ f g) #'(procedure-rename
                (λ x (bind (apply return x) >>= g >>= f))
                'composed/m)]
    [(_ f ...) (with-syntax ([(s ...) (local-expand 
                                       #'(seq f ...) 'expression #f)])
                 #'(procedure-rename
                    (λ x (bind (apply return x) >>= s ...))
                    'composed/m))]))

(define-syntax (seq stx)
  (syntax-case stx (>>=)
    [(_ f g) #'(g >>= f)]
    [(_ g ... f)  (with-syntax ([(expanded-seq ...) (local-expand 
                                               #'(seq g ...) 'expression #f)])
                    #`(f >>= expanded-seq ...))]))

;; lifting the function
(define (lift f) 
  (compose1 return f)
  #;(if (lifted? f) 
      f
      ((set-tag 'lifted (or (object-name f) 'λ)) (compose1 return f))))

#;(define (lifted? f) (check-tag 'lifted f))

(define-syntax (lift/m stx)
  (syntax-case stx ()
    [(_ f x ...) 
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

;; monadic map
(define (map/m f ms)
  (for/fold ((res mzero)) ((x ms))
    (mplus (f x) res)))

