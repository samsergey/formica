#lang scribble/doc

@(require (for-label formica))

@(require
  scribble/manual
  scribble/eval)

@(define formica-eval
   (let ([sandbox (make-base-eval)])
     (sandbox '(require formica))
     sandbox))

@title[#:tag "arity"]{Managing the function arity}

@defmodule[formica/arity]

The bindings documented in this section are provided by the @racketmodname[formica/arity], 
@racketmodname[formica/tools] and @racketmodname[formica] modules.


@defproc[(polyadic? [f Any]) boolean?]
Returns @racket[#t] if @racket[_f] is polyadic function, and @racket[#f] otherwise.

Examples:
@interaction[#:eval formica-eval
  (polyadic? (case-lambda
               [(x) x]
               [(x y) (+ x y)]))
  (define (f x (y 2)) '(f x y))
  (polyadic? f)
  (polyadic? cons)
  (polyadic? +)]


@defproc[(variadic? [f Any]) boolean?]
Returns @racket[#t] if @racket[_f] is variadic function, and @racket[#f] otherwise.

Examples:
@interaction[#:eval formica-eval
  (variadic? (case-lambda
               [(x) x]
               [(x . y) (apply + x y)]))
  (define (f . x) (cons 'f x))
  (variadic? f)
  (variadic? cons)
  (variadic? +)]

@defproc[(nullary? [f Any]) boolean?]
Returns @racket[#t] if arity of @racket[_f] includes 0, and @racket[#f] otherwise.

Examples:
@interaction[#:eval formica-eval
  (nullary? (case-lambda
              [() 0]
              [(x) x]
              [(x . y) (apply + x y)]))
  (define (f . x) (cons 'f x))
  (nullary? f)
  (nullary? cons)
  (nullary? +)]

@defproc[(unary? [f Any]) boolean?]
Returns @racket[#t] if arity of @racket[_f] includes 1, and @racket[#f] otherwise.

Examples:
@interaction[#:eval formica-eval
  (unary? (case-lambda
              [() 0]
              [(x) x]
              [(x . y) (apply + x y)]))
  (define (f x . y) (list* 'f x y))
  (unary? f)
  (unary? not)
  (unary? cons)
  (unary? +)]

@defproc[(binary? [f Any]) boolean?]
Returns @racket[#t] if arity of @racket[_f] includes 2, and @racket[#f] otherwise.

Examples:
@interaction[#:eval formica-eval
  (binary? (case-lambda
              [() 0]
              [(x y) (+ x y)]
              [(x . y) (apply + x y)]))
  (define (f x . y) (list* 'f x y))
  (binary? f)
  (binary? not)
  (binary? cons)
  (binary? +)]

@defproc[(min-arity [f Fun]) (or/c 0 positive?)]
Returns the minimal arity of a function @racket[#t].

Examples:
@interaction[#:eval formica-eval
  (min-arity (case-lambda
              [() 0]
              [(x y) (+ x y)]))
  (define (f x . y) (list* 'f x y))
  (min-arity f)
  (min-arity not)
  (min-arity cons)
  (min-arity +)]

@defproc[(max-arity [f Fun]) (or/c 0 positive? +inf.0)]
Returns the maximal arity of a function @racket[#t].

Examples:
@interaction[#:eval formica-eval
  (max-arity (case-lambda
              [() 0]
              [(x y) (+ x y)]))
  (define (f x . y) (list* 'f x y))
  (max-arity f)
  (max-arity not)
  (max-arity cons)
  (max-arity +)]

@defproc[(inherit-arity [f Fun]) (Fun -> Fun)]
Returns the operator which reduces the arity of given function to arity of @racket[_f], if it is possible.

Examples:
@interaction[#:eval formica-eval
  (procedure-arity
   ((inherit-arity cons) (case-lambda
                           [() 0]
                           [(x . y) (apply + x y)])))
  (define (f . x) (cons 'f x))
  (procedure-arity ((inherit-arity +) f))]