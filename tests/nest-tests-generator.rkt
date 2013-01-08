#lang racket/base
(require (except-in "test-utils.rkt" sufficient-argument-list)
         "../tools.rkt"
         racket/list)

(define (sufficient-argument-list f)
  (cond
    [(procedure? f) (sufficient-argument-list (procedure-arity f))]
    [(list? f) (append-map sufficient-argument-list f)]
    [(arity-at-least? f) (sufficient-argument-list (list (arity-at-least-value f)
                                                         (+ 1 (arity-at-least-value f))))]
    [(integer? f) (list (range 1 (+ 1 f)))]))

(define (insufficient-argument-list f)
  (define l (sufficient-argument-list f)) 
  (if (null? l) l (cdr l)))

(define (redundant-argument-list f)
  (cons 0 (sufficient-argument-list f)))

(define out (open-output-file "automatic-nest-tests.rkt" #:exists 'replace))

(current-output-port out)
(displayln "#lang racket/base")
(displayln "(require \"../private/tools/nest.rkt\" \"test-utils.rkt\")") 

(displayln "(define (two-functions)")
(displayln "(test-case \"nesting two functions\"")
(for* ([f test-functions]
       #:when (not (nullary? f))
       [g test-functions]    
       [F (in-value (∘ f g))]
       [x (sufficient-argument-list F)]
       [nf (in-value  (object-name f))]
       [ng (in-value  (object-name g))])
  (unless (nullary? F)
    (let ([y (build-list (- (min-arity F) 1) values)])
      (displayln (format "  (check-exn exn:fail:contract? (λ () ~a))" `((∘ ,nf ,ng) ,@y)))))
  (unless (variadic? F)
    (let ([y (build-list (+ (max-arity F) 1) values)])
      (displayln (format "  (check-exn exn:fail:contract? (λ () ~a))" `((∘ ,nf ,ng) ,@y)))))
  (displayln (format "  (check-equal? ~a\t~s)" `((∘ ,nf ,ng) ,@x)  (apply F x))))
(displayln "))")
(displayln "(provide two-functions)")


(displayln "(define (right-identity)")
(displayln "(test-case \"id is the right unit\"")
(for* ([f test-functions]
       #:when (not (nullary? f))
       [F (in-value (∘ f (λ(x)x)))]
       [x (sufficient-argument-list F)]
       [nf (in-value (object-name f))])
  (displayln (format "  (check-equal? ~a\t~a)" `((∘ ,nf (λ(x)x)) ,@x)  (apply f x))))
(displayln "))")
(displayln "(provide right-identity)")

(displayln "(define (left-identity)")
(displayln "(test-case \"id is the left unit\"")
(for* ([f test-functions]
       [F (in-value (∘ (λ(x)x) f))]
       [x (sufficient-argument-list F)]
       [nf (in-value (object-name f))])
  (displayln (format "  (check-equal? ~a\t~a)" `((∘ (λ(x)x) ,nf) ,@x)  (apply f x))))

(displayln "))")
(displayln "(provide left-identity)")

(displayln "(define (associativity)")
(displayln "(test-case \"associativity for fixed-arity functions\"")  
(for* ([f test-functions]
       #:when (not (nullary? f))
       [g test-functions]
       #:when (not (or (nullary? g)
                       (variadic? g)))
       [h test-functions]
       [F1 (in-value (∘ f (∘ g h)))]
       [F2 (in-value (∘ (∘ f g) h))]
       [x (sufficient-argument-list F1)]
       [nf (in-value (object-name f))]
       [ng (in-value (object-name g))]
       [nh (in-value (object-name h))])
  (displayln (format "  (check-equal? ~a\t~a)" 
                     `((∘ ,nf (∘ ,ng ,nh)) ,@x)  
                     `((∘ (∘ ,nf ,ng) ,nh) ,@x))))
(displayln "))")
(displayln "(provide associativity)")


(close-output-port out)
