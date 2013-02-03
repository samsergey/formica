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
         racket/provide-syntax
         racket/require-syntax
         (for-syntax racket/base 
                     racket/match
                     racket/list
                     racket/syntax
                     "../tools/tags.rkt"
                     "hold.rkt")
         (for-syntax (for-syntax racket/base racket/syntax))
         (prefix-in part: "../syntax/partial-app.rkt")
         "../tools/tags.rkt"
         "hold.rkt"
         "../types/infix-arrow.rkt"
         "../types/type-checking.rkt")

(provide 
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
(define (add-to-formals p)
  (define (include x s)
    (match s
      ['() (list x)]
      [(and s (cons h t)) (if (equal? (object-name x)
                                      (object-name h))
                              s
                              (cons h (include x t)))]))
  (formals (include p (formals))))

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
  
  (syntax-case stx (.. ?)
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
                [(id x (... ...)) (part:#%app (hold 'id n) x (... ...))]
                [id (hold 'id n)]))
            (define-syntax (id: stx)
              ; the contracts for formal applications
              (syntax-case stx (.. ?)
                [(id: c (... ...) (? x (... ...))) 
                 (with-syntax ([((oc (... ...)) (... ...)) 
                                (add-optional (syntax->list #'(c (... ...)))
                                              (syntax->list #'(x (... ...))))])
                   #'(procedure-rename 
                      (or/c (id: oc (... ...)) (... ...))
                      (string->symbol (format "~a" '(id: c (... ...) (? x (... ...)))))))]
                [(id: c ..) #'(procedure-rename                               
                               (cons/c 'id (listof c))
                               (string->symbol (format "~a" '(id: c ..) )))]
                [(id: c (... ...) x ..) #'(procedure-rename                                           
                                           (foldr cons/c (listof x) (list 'id c (... ...)))
                                           (string->symbol (format "~a" '(id: c (... ...) x ..))))]
                [(id: c (... ...)) #'(procedure-rename 
                                      (list/c 'id c (... ...))
                                      (string->symbol (format "~a" '(id: c (... ...)))))]))
            ; the predicate for formal applications
            (define id?
              (match-lambda predicate-rules ... (_ #f)))
            ; new predicate in the list of functions declared to be formal
            (add-to-formals id?))))]
    
    
    [(_ (id #:contract (cntr ...))) 
     ; compile-time syntax check
     (unless (symbol? (syntax-e #'id)) 
       (raise-syntax-error 'define-formal 
                           "expected a symbol as a formal function name" 
                           (syntax-e #'id)))
     (let* ([str (symbol->string (syntax-e #'id))]
            [str? (string->symbol (string-append str "?"))]
            [str: (string->symbol (string-append str ":"))])
       (with-syntax* 
        ([id? (datum->syntax #'id str? #'id)]
         [id: (datum->syntax #'id str: #'id)]
         [f #'(contract (.->. cntr ... id?) (hold 'id) 'id 'id 'id #f)])
        #'(begin             
            (define-syntax (id: stx)
              ; the contracts for formal applications
              (syntax-case stx (.. ?)
                [(id: c (... ...) (? x (... ...))) 
                 (with-syntax ([((oc (... ...)) (... ...)) 
                                (add-optional (syntax->list #'(c (... ...)))
                                              (syntax->list #'(x (... ...))))])
                   #'(procedure-rename 
                      (or/c (id: oc (... ...)) (... ...))
                      (string->symbol (format "~a" '(id: c (... ...) (? x (... ...)))))))]
                [(id: c ..) #'(procedure-rename                               
                               (cons/c 'id (listof c))
                               (string->symbol (format "~a" '(id: c ..) )))]
                [(id: c (... ...) x ..) #'(procedure-rename                                           
                                           (foldr cons/c (listof x) (list 'id c (... ...)))
                                           (string->symbol (format "~a" '(id: c (... ...) x ..))))]
                [(id: c (... ...)) #'(procedure-rename 
                                      (list/c 'id c (... ...))
                                      (string->symbol (format "~a" '(id: c (... ...)))))]))
            
            (define (id? x)
              ; the predicate for formal applications
              (is x (id: cntr ...)))
            
            (define-match-expander id
              ; the match expander (f: ...)
              (syntax-rules () 
                [(id x (... ...)) (list (quote id) x (... ...))])
              ; formal function outside the match form
              (syntax-id-rules ()
                [(id x (... ...)) (part:#%app f x (... ...))]
                [id f]))
            
            ; new predicate in the list of functions declared to be formal
            (add-to-formals id?))))]
    
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
                 [(id x (... ...)) (part:#%app (hold 'id) x (... ...))]
                 [id (hold 'id)]))
             (define-syntax (id: stx)
               ; the contracts for formal applications
               (syntax-case stx (.. ?)
                 [(id: c (... ...) (? x (... ...))) 
                  (with-syntax ([((oc (... ...)) (... ...)) 
                                 (add-optional (syntax->list #'(c (... ...)))
                                               (syntax->list #'(x (... ...))))])
                    #'(procedure-rename 
                       (or/c (id: oc (... ...)) (... ...))
                       (string->symbol (format "~a" '(id: c (... ...) (? x (... ...)))))))]
                 [(id: c ..) #'(procedure-rename                               
                                (cons/c 'id (listof c))
                                (string->symbol (format "~a" '(id: c ..) )))]
                 [(id: c (... ...) x ..) #'(procedure-rename                                           
                                            (foldr cons/c (listof x) (list 'id c (... ...)))
                                            (string->symbol (format "~a" '(id: c (... ...) x ..))))]
                 [(id: c (... ...)) #'(procedure-rename 
                                       (list/c 'id c (... ...))
                                       (string->symbol (format "~a" '(id: c (... ...)))))]))
             ; the predicate for formal applications
             (define id? 
               (match-lambda
                 [(cons 'id _) #t]
                 [_ #f]))
             ; new predicate in the list of functions declared to be formal
             (add-to-formals id?))))]
    [(_ ids ...) #'(begin (define-formal ids) ...)]))

(define-for-syntax (add-optional s xlist)
  (reverse 
   (for/fold ([res (list s)]) ([x (in-list xlist)])
     (cons (append (car res) (list x)) res))))

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


(define-provide-syntax (formal-out stx)
  (syntax-case stx ()
    [(_ id) (with-syntax ([o (make-out-combination #'id)]
                          [frm #'formals])
              #'(combine-out o frm))]
    [(_ ids ...) (with-syntax ([(o ...) (map make-out-combination (syntax->list #'(ids ...)))]
                               [frm #'formals])
                   #'(combine-out o ... frm))]))

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
        #'(only-in mod id id? id:))))

#;(define-require-syntax (formal-in stx)
    (syntax-case stx ()
      [(_ m id) ((make-in-combination #'m) #'id)]
      [(_ m ids ...) (with-syntax ([(i ...) 
                                    (map (make-in-combination #'m) 
                                         (syntax->list #'(ids ...)))])
                       #'(combine-in i ...))]))
