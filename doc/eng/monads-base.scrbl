#lang scribble/doc

@(require (for-label formica))

@(require
  scribble/manual
  scribble/eval)

@(define formica-eval
   (let ([sandbox (make-base-eval)])
     (sandbox '(require formica))
     sandbox))

@title{Basic operations with monads}

@declare-exporting[formica]

@section{Constructing monads}

Following two functions return anonymous monads which could be used for creation of parameterized monads.

@defproc[(monad [#:return return (Any -> Any)]
                [#:bind bind (Any (Any-> Any) -> Any)]) monad?]
Returns a monad with given @racket[return] and @racket[bind] functions.

@defproc[(monad-plus [#:return return (Any -> Any)]
                [#:bind bind (Any (Any-> Any) -> Any)]
                [#:mzero mzero Any]
                [#:mplus mplus (Any Any -> Any)]) monad-plus?]
Returns an additive monad with given @racket[return], @racket[bind], @racket[mplus] functions and neutral element @racket[mzero].

@defform[(define-monad id 
           #:return return 
           #:bind bind)
         #:contracts ([return (Any -> Any)]
                      [bind (Any (Any-> Any) -> Any)])]
Defines a monad with name @racket[_id] and given @racket[return] and @racket[bind] functions.

@defform[(define-monad-plus id 
           #:return return 
           #:bind bind 
           #:mzero mzero
           #:mplus mplus)
         #:contracts ([return (Any -> Any)]
                      [bind (Any (Any-> Any) -> Any)]
                      [mzero Any]
                      [mplus (Any Any -> Any)])]
Defines an additive monad with name @racket[_id] and given @racket[return], @racket[bind], @racket[mplus] functions and neutral element @racket[mzero].

The keywords in definition forms are used for clarity and can't be omitted, however they could be given in arbitrary order.

@bold{Examples:}

A simple container monad:
@defs+int[#:eval formica-eval
  ((define-formal (m 1))
   (define-monad M
     #:return m
     #:bind (/. (m x) f --> (f x))))
  M
  (using-monad M)]

Basic binding:
@interaction[#:eval formica-eval
  (return 'a)
  (define-formal f g)
  (bind (m 'a) >>= (lift f))
  (bind (m 'a) >>= (lift f) >>= (lift g))
  (do [x <-: 'a]
      [y <-: (f x 'b)]
      (return (g y)))]

Monadic functions:
@interaction[#:eval formica-eval
  ((compose/m (lift f) (lift g)) 'a)
  (lift/m f (m 'a) (m 'b))]

A simple additive monad (equivalent to Maybe):
@defs+int[#:eval formica-eval
  ((define-formal (m 1))
   (define-monad-plus A/M
     #:return (/. 'z --> 'z
                   x --> (m x))
     #:bind (/. 'z    f --> 'z
                (m x) f --> (f x))
     #:mzero 'z
     #:mplus (/. 'z _ --> 'z
                 _ 'z --> 'z
                 x  _ --> x)))
  A/M
  (using-monad A/M)]

Basic binding:
@interaction[#:eval formica-eval
  (return 'a)
  (return 'z)
  (define-formal f g)
  (bind (m 'a) >>= (lift f))
  (bind 'z >>= (lift f) >>= (lift g))
  (do [x <-: 'a]
      [y <-: (f x 'b)]
      (return (g y)))]
Guarding:
@interaction[#:eval formica-eval
  (bind (m 2) >>= (guardf even?) >>= (lift g))
  (bind (m 2) >>= (guardf odd?) >>= (lift g))
  (do [x <-: 2]
      (guard (odd? x))
      [y <-: (f x 'b)]
      (return (g y)))
  (collect (g y)
    [x <-: 2]
    (odd? x)
    [y <-: (f x 'b)])]

Monadic functions:
@interaction[#:eval formica-eval
  ((compose/m (lift f) (lift g)) 'a)
  (lift/m f (m 'a) (m 'b))
  (lift/m f (m 'a) 'z)]

@defproc*[([(monad? [v Any]) Bool]
           [(monad-plus? [v Any]) Bool])]
Return @racket[#t] if @racket[_v] is monad or additive monad, respectively. Otherwise return @racket[#f]. Any additive monad satisfies the @racket[monad?] predicate.

Examples:
@interaction[#:eval formica-eval
  (monad? M)
  (monad? A/M)
  (monad-plus? M)
  (monad-plus? A/M)]

@section{Using monads}

All monads share the same syntax for binding and monadic functions. At a given time only one monad, called @deftech{currently used monad} could be used.

@defparam[using-monad m monad?]
Defines the @tech{currently used monad}.

Examples:
@interaction[#:eval formica-eval
 (using-monad)
 (using-monad Id)
 (using-monad)]


@defform[(using m expr ...) #:contracts ([m monad?])]
Evaluates @racket[_expr ...] using monad @racket[_m] as @tech{currently used monad}.

Examples:
@interaction[#:eval formica-eval
 (using Id 
   (do [x <- 5]
       [x <- (+ x 6)]
       (return x)))
 (using List
   (do [x <- '(1 2 3)]
       [x <-: (+ x 6)]
       (return x)))]

@defproc[(return [x Any]) any]
The unit function of @tech{currently used monad}.

Examples:
@interaction[#:eval formica-eval
 (using M (return 5))
 (using Id (return 5))
 (using List (return 1 2 3))]

@defform/subs[#:literals (>>= >>)(bind m <arr> f [ <arr> fs ...]) 
([<arr> >>= >>])]
Binding in the @tech{currently used monad}. 

@itemize{
  @item{@racket[_m] @defidform/inline[>>=] @racket[_f] binding of function @racket[_f] and value @racket[_m].}
  @item{@racket[_expr1] @defidform/inline[>>] @racket[_expr2] sequential computation of @racket[_expr1] и @racket[_expr2].
         Makes sence only if @racket[_expr1] has side effects.}}

The form mimics the @emph{Haskell} syntax for monadic binding:

@centered{@emph{Haskell:} @racket[m >>= f >> g ...]}

@centered{@emph{Formica:} @racket[(bind m >>= f >> g ...)]}

Examples:
@interaction[#:eval formica-eval
 (using Id 
   (bind 5 >>= sqr))
 (using M 
   (bind (m 5) >>= (lift sqr)))
 (using List
   (bind '(1 2 3) >>= (lift sqr)))]

@interaction[#:eval formica-eval
 (using Id 
   (bind 8 >>= displayln >> 5 >>= sqr))
  (using M 
   (bind (m 8) >>= (lift displayln) >> (m 5) >>= (lift sqr)))
 (using List
   (bind '(1 2 3) >>= (lift displayln) >> '(4 5 6) >>= (lift sqr)))]

When called without subform, @racket[bind] evaluates to a binding function of the @tech{currently used monad}.
@interaction[#:eval formica-eval
 (using M
   (apply bind (list (m 'x) f)))
 (using List
   (apply bind (list '(a b c) (lift f))))]

@defthing[mzero Any]
A zero element of the @tech{currently used monad}.

Examples:
@interaction[#:eval formica-eval
 (using List mzero)
 (using A/M mzero)
 (using M mzero)]


@defproc[(mplus [x Any] [y Any]) Any]
A monadic plus operation of the @tech{currently used monad}.

Examples:
@interaction[#:eval formica-eval
 (using List (mplus '(1 2 3) '(3 4 5)))
 (using A/M (map mplus '(x y z) '(y z x)))]