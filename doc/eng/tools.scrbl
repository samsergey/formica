#lang scribble/doc

@(require (for-label formica))

@(require
  scribble/manual
  scribble/eval)

@(define formica-eval
   (let ([sandbox (make-base-eval)])
     (sandbox '(require formica))
     sandbox))

@title[#:tag "tools"]{Comparison and ordering}

@declare-exporting[formica/tools]

The bindings documented in this section are provided by the @racketmodname[formica/tools] and @racketmodname[formica] modules.
        
@defproc*[([(eq? [x Any] [y Any] ...+) boolean?]
           [(equal? [x Any] [y Any] ...+) boolean?])]
Same as @racket[eq?] and @racket[equal?] proveded @racketmodname[racket/base], but if more then two arguments are given, arguments are compared pairwise from left to right.

@defproc[(different? [x Any] [y Any] ...+) boolean?]
Returns @racket[#t] if @racket[_x] and @racket[_y] are not @racket[equal?], and @racket[#f] otherwise. If more then two arguments are given, they are compared pairwise.

Examples:
@interaction[#:eval formica-eval
  (different? 1 1)
  (different? 1 2)
  (different? 1 1.0)
  (different? 1 1 1 1 2 1 1)]

@defproc[(almost-equal? [x Any] [y Any] ...+) boolean?]
@defthing[≈ almost-equal?]
Returns @racket[#t] if @racket[_x] and @racket[_y] are @racket[equal?], or for numeric values if the magnitude of difference between @racket[_x] and @racket[_y] is less then @racket[(tolerance)]. Returns @racket[#f] otherwise.

If lists and pairs are compared, they are @racket[almost-equal?] if they have the same structure and all entries at corresponding positions are @racket[almost-equal?].

If more then two arguments are given, they are compared pairwise from left to right.

Function @racket[almost-equal?] has an alias: @racket[≈]
(could be entered as @litchar{\approx} + Alt @litchar{\}).

Examples:
@interaction[#:eval formica-eval
  (≈ 'x 'x)
  (≈ 1 'x)
  (≈ 1 1.)
  (≈ 1/2 (+ 0.5 1e-15))
  (≈ 1/2 (+ 0.5 1e-16))
  (≈ 1/2 (+ 0.5 0+1e-16i))
  (≈ 0.5 1/2 (+ 0.5 0+1e-16i))
  (≈ '(1 (2 3)) '(1 (2.000000000000001 3)))]

@defparam[tolerance x real?]
Defines an relative tolerance used by the @racket[almost-equal?] predicate. The default value is @racket[5e-16].

Examples:
@interaction[#:eval formica-eval
  (parameterize ([tolerance 0.01])
    (and (≈ 1 1.001)
         (≈ 10 10.01)
         (≈ 1e23 1.001e23)
         (≈ 1e-23 1.001e-23)
         (≈ 0 0.001)))]

In the last case it is impossible to use the relative tolerance, so @racket[(tolerance)] is interpreted as absolute.

