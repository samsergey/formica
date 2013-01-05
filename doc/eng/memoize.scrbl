#lang scribble/doc

@(require (for-label formica))

@(require
  scribble/manual
  scribble/eval)

@(define formica-eval
   (let ([sandbox (make-base-eval)])
     (sandbox '(require formica))
     sandbox))

@title[#:tag "memo"]{Memoization}

@declare-exporting[formica/memoize]
The bindings documented in this section are provided by the @racket[formica/memoize] library and @racket[formica] language.

@defproc[(memoized [f Fun]) memoized?]
Returns memoized equivalent of the function @racket[_f].

Examples:
@interaction[#:eval formica-eval
  (define g (memoized current-inexact-milliseconds))
  (g)
  (current-inexact-milliseconds)
  (g)]

@defproc[(memoized? [f Fun]) Bool]
Returns @racket[#t] if @racket[_f] is a memoized function, and @racket[#f] otherwise.

Examples:
@interaction[#:eval formica-eval
  (define g (memoized +))
  (memoized? g)
  (memoized? +)
  (g 2 3)]

@defform*[((define/memo (f var ...) body ...)
          (define/memo f fun))]
Defines memoized function @racket[_f]. If formal variables are not given, 
the right-hand side of definition @racket[_fun] should evaluate to a function.

Examples:
@interaction[#:eval formica-eval
  (define/memo (f x) 
    (display x) 
    (* x x))
  (memoized? f)
  (f 2)
  (f 2)
  (f 3)]

@defform[(define/memo/c (f var ...) body)]
Defines memoized function @racket[_f] using @tech{point-free notation}.
