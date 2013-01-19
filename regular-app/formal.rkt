#lang racket/base
;;______________________________________________________________
;;                   ______ 
;;                  (  //   _____ ____   .  __  __
;;                   ~//~ ((_)// // / / // ((_ ((_/_
;;                 (_//
;;..............................................................
; Provides formal functions.
;;;=============================================================
(require racket/match 
         racket/contract
         racket/syntax
         (for-syntax racket/base
                     racket/match
                     racket/list
                     racket/syntax
                     "../private/tools/tags.rkt"
                     "../private/formal/hold.rkt")
         "../private/tools/tags.rkt"
         "../private/formal/hold.rkt")

(provide 
 (all-from-out "../private/formal/hold.rkt")
 (contract-out
  (formals (parameter/c (listof predicate/c)))
  (formal? predicate/c)
  (n/f-list? predicate/c)
  (n/f-pair? predicate/c))
 define-formal
 formal
 formal-out 
 #;formal-in)


;;--------------------------------------------------------------------------------
;; The parameter which keeps track of functions declared to be formal
;;--------------------------------------------------------------------------------
(define formals (make-parameter '()))

;;--------------------------------------------------------------------------------
;; Helper function which creates a list of rules for match expander f:
;;--------------------------------------------------------------------------------
(define-for-syntax (make-match-rules id id: n)
  (with-syntax ([id* id]
                [id:* id:])
    (match n
      [(? integer? n) 
       (with-syntax ([(tmp ...) (generate-temporaries (make-list n 'x))])
         (list #'[(id:* tmp ...) (list (quote id*) tmp ...)]))]
      [(list 'arity-at-least n) 
       (with-syntax ([(tmp ...) (generate-temporaries (make-list (add1 n) 'x))])
         (list #'[(id:* tmp ... (... ...)) (list (quote id*) tmp ... (... ...))]))]
      [(cons n m) (append (make-match-rules id id: n) (make-match-rules id id: m))]
      [else '()])))

;;--------------------------------------------------------------------------------
;; Helper function which creates a list of rules for the predicate f?
;;--------------------------------------------------------------------------------
(define-for-syntax (make-predicate-rules id n)
  (with-syntax ([id* id])
    (match n
      [(? integer? n) 
       (with-syntax ([(tmp ...) (generate-temporaries (make-list n 'x))])
         (list #'[(id* tmp ...) #t]))]
      [(list 'arity-at-least n) 
       (with-syntax ([(tmp ...) (generate-temporaries (make-list (add1 n) 'x))])
         (list #'[(id* tmp ... (... ...)) #t]))]
      [(cons n m) (append (make-predicate-rules id n) (make-predicate-rules id m))]
      [else '()])))

;;--------------------------------------------------------------------------------
;; The definition of formal functions
;;--------------------------------------------------------------------------------
(define-syntax (define-formal stx)
  (syntax-case stx ()
    [(_ (id n)) 
     ; compile-time syntax check
     (unless (symbol? (syntax-e #'id)) 
       (raise-syntax-error 'define-formal 
                           "expected a symbol as a formal function name" 
                           #'(id n)))
     (let* ([str (symbol->string (syntax-e #'id))]
            [str? (string->symbol (string-append str "?"))]
            [str: (string->symbol (string-append str ":"))]
            [dn (syntax->datum #'n)])
       (with-syntax* 
        ([id? (datum->syntax #'id str? #'id)]
         [id: (datum->syntax #'id str: #'id)] 
         [(match-rules ...) (make-match-rules #'id #'id dn)]
         [(predicate-rules ...) (make-predicate-rules #'id dn)])
        #'(begin
            ; declaration-time syntax check
            (unless (procedure-arity? n) 
              (raise-syntax-error 'define-formal 
                                  "invalid arity specification" 
                                  #'(id n)))
            (define-match-expander id 
              ; the match expander (f: ...)
              (syntax-rules () 
                match-rules ... 
                [(id x (... ...)) (list (quote id) x (... ...))])
              ; formal function outside the match form
              (syntax-id-rules ()
                [(id x (... ...)) ((hold 'id n) x (... ...))]
                [id (hold 'id n)]))
            (define-syntax id:
              ; the contracts for formal applications
              (syntax-rules (..)
                [(id: c ..) (cons/c 'id (listof c))]
                [(id: c (... ...)) (procedure-reduce-arity
                                    (list/c 'id c (... ...))
                                    n)]))
            ; the predicate for formal applications
            (define id?
              (match-lambda predicate-rules ... (_ #f)))
            ; new predicate in the list of functions declared to be formal
            (formals (cons id? (formals))))))]
    [(_ id) 
     ; compile-time syntax check
     (unless (symbol? (syntax-e #'id)) 
       (raise-syntax-error 'define-formal 
                           "expected a symbol as a formal function name" 
                           (syntax-e #'id)))
     (let* ([str (symbol->string (syntax-e #'id))]
            [str? (string->symbol (string-append str "?"))]
            [str: (string->symbol (string-append str ":"))])
       (with-syntax ([id? (datum->syntax #'id str? #'id)]
                     [id: (datum->syntax #'id str: #'id)])
         #'(begin
             (define-match-expander id
               ; the match expander (f: ...)
               (syntax-rules () 
                 [(id x (... ...)) (list (quote id) x (... ...))])
               ; formal function outside the match form
              (syntax-id-rules ()
                [(id x (... ...)) ((hold 'id) x (... ...))]
                [id (hold 'id)]))
             (define-syntax id:
             ; the contracts for formal applications
               (syntax-rules (..)
                 [(id: c ..) (cons/c 'id (listof c))]
                 [(id: c (... ...)) (list/c 'id c (... ...))]))
             ; the predicate for formal applications
             (define id? 
               (match-lambda
                 [(cons 'id _) #t]
                 [_ #f]))
             ; new predicate in the list of functions declared to be formal
             (formals (cons id? (formals))))))]
    [(_ ids ...) #'(begin (define-formal ids) ...)]))

;;--------------------------------------------------------------------------------
;; The predicate that returns #t for the formal applications
;;--------------------------------------------------------------------------------
(define (formal? x)
  (ormap (λ(f)(f x)) (formals)))

;;--------------------------------------------------------------------------------
;; The match expander for general formal application
;; This expander may return the head of the formal application
;;--------------------------------------------------------------------------------
(define-match-expander formal
  (syntax-rules ()
    [(formal (h x ...)) (? formal? (list h x ...))]))

;;--------------------------------------------------------------------------------
;; The predicate that returns #t for lists which are not formal applications
;;--------------------------------------------------------------------------------
(define (n/f-list? x)
  (and (list? x)
       (not (formal? x))))

;;--------------------------------------------------------------------------------
;; The predicate that returns #t for pairs which are not formal applications
;;--------------------------------------------------------------------------------
(define (n/f-pair? x)
  (and (pair? x)
       (not (formal? x))))

;;--------------------------------------------------------------------------------
;; The formal-out form
;;--------------------------------------------------------------------------------
(require (for-syntax racket/provide-transform
                     racket/require-transform))

(define-for-syntax (make-out-combination stx)
  (let* ([str (symbol->string (syntax-e stx))]
         [str? (string->symbol (string-append str "?"))]
         [str: (string->symbol (string-append str ":"))])
    (with-syntax ([id stx]
                  [id? (datum->syntax #'id str? #'id)]
                  [id: (datum->syntax #'id str: #'id)])
      #'(combine-out id id? id:))))

(define-syntax formal-out
  (make-provide-pre-transformer 
   (λ (stx modes)
     (syntax-case stx ()
       [(_ id) (make-out-combination #'id)]
       [(_ ids ...) (with-syntax ([(o ...) (map make-out-combination (syntax->list #'(ids ...)))])
                        #'(combine-out o ...))]))))


;;--------------------------------------------------------------------------------
;; The formal-in form
;; To be done
;;--------------------------------------------------------------------------------
#;(define-for-syntax ((make-in-combination m) stx)
  (let* ([str (symbol->string (syntax-e stx))]
         [str? (string->symbol (string-append str "?"))]
         [str: (string->symbol (string-append str ":"))])
    (with-syntax ([mod m] [id stx]
                  [id? (datum->syntax #'id str? #'id)]
                  [id: (datum->syntax #'id str: #'id)])
      (expand-import #'(only-in mod id id? id:)))))

#;(define-syntax formal-in
  (make-require-transformer 
   (λ (stx)
     (syntax-case stx ()
       [(_ m id) ((make-in-combination #'m) #'id)]
       [(_ m ids ...) (with-syntax ([(i ...) 
                                     (map (make-in-combination #'m) 
                                          (syntax->list #'(ids ...)))])
                        (values #'(combine-in i ...) #'m))]))))

