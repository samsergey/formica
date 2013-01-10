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

@defform[(define-monad id 
           #:return return 
           #:bind bind
           [#:type type])
         #:contracts ([return (Any -> Any)]
                      [bind (Any (Any-> Any) -> Any)]
                      [type contract?])]
Defines a monad with name @racket[_id] and given @racket[_return] and @racket[_bind] functions.

All keywords except @racket[#:type] in definition forms are used for clarity and can't be omitted, however they could be given in arbitrary order.

If the @racket[_type] is given monadic values are restricted to satisfy the given contract. The concept of monads fits the type system very well, providing type consistency in sequential computations. The type specification helps to debug programs using monads and makes them more robust.

@defform[(define-monad-plus id 
           #:return return 
           #:bind bind 
           #:mzero mzero
           #:mplus mplus
           [#:type type])
         #:contracts ([return (Any -> Any)]
                      [bind (Any (Any-> Any) -> Any)]
                      [mzero Any]
                      [mplus (Any Any -> Any)]
                      [type contract?])]
Like @racket[define-monad], but defines an additive monad with name @racket[_id] and given @racket[_return], @racket[_bind], @racket[_mplus] functions and zero element @racket[_mzero].

@bold{Examples:}

A simple container monad:
@defs+int[#:eval formica-eval
  ((define-formal (m 1) f g)
   (define-monad M
     #:return m
     #:bind (/. (m x) f --> (f x))))
  M
  (using-monad M)]

Basic binding:
@interaction[#:eval formica-eval
  (return 'a)
  (bind (m 'a) >>= (lift f))
  (bind (m 'a) >>= (lift f) >>= (lift g))
  (do [x <-: 'a]
      [y <-: (f x 'b)]
      (return (g y)))]

Monadic functions:
@interaction[#:eval formica-eval
  ((compose/m (lift f) (lift g)) 'a)
  (lift/m f (m 'a) (m 'b))]

A simple additive monad (equivalent to @tt{Maybe}). In this example the type of monadic values is specified.
@defs+int[#:eval formica-eval
  ((define-formal (m 1))
   (define-type A/M? (m: Any) 'z)
   (define-monad-plus A/M
     #:type A/M?
     #:return (/. 'z --> 'z
                   x --> (m x))
     #:bind (/. 'z    f --> 'z
                (m x) f --> (f x))
     #:mzero 'z
     #:mplus (/. 'z x --> x
                  x _ --> x)))
  A/M
  (using-monad A/M)]

Basic binding:
@interaction[#:eval formica-eval
  (return 'a)
  (return 'z)
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
  (lift/m f (m 'a) 'z)
  (sum/m '(z z z a b))]

The definition of the @racket[A/M] monad declares the type of monadic values. It makes the error reports more clear.
@interaction[#:eval formica-eval
  (bind 'a >>= (lift f))
  (bind (m 'a) >>= f)
  (lift/m f (m 'a) 'b)]

Monads are first-class objects in Formica. Following two functions return anonymous monads which could be used to create parameterized monads as mixins.

@defproc[(monad [#:return return (Any -> Any)]
                [#:bind bind (Any (Any-> Any) -> Any)]
                [#:type type contract? #f]) monad?]
@defproc[(monad-plus [#:return return (Any -> Any)]
                [#:bind bind (Any (Any-> Any) -> Any)]
                [#:mzero mzero Any]
                [#:mplus mplus (Any Any -> Any)]
                [#:type type contract? #f]) monad-plus?]
Return a monad or an additive monad with given @racket[return], @racket[bind] (complemented by @racket[mplus] and @racket[mzero] in case of additive monad). If the @racket[type] is given monadic values are restricted to satisfy the given contract.

Example:

A monad with parameterized type (@tt{Maybe a}):
@defs+int[#:eval formica-eval
  ((define-formal Maybe Just)
   (define-type (Maybe? a) (Just: a) 'Nothing)
   (define (Maybe a)
     (monad-plus
      #:type (Maybe? a)
      #:return (/. 'Nothing --> 'Nothing
                    x       --> (Just x))
      #:bind (/. 'Nothing f --> 'Nothing
                 (Just x) f --> (f x))
      #:mzero 'Nothing
      #:mplus (/. 'Nothing x --> x
                   x       _ --> x))))
  (using-monad (Maybe Int))
  (bind (Just 2) >>= (lift sqr) >>= (lift (* 2)))
  (bind 'Nothing >>= (lift sqr) >>= (lift (* 2)))  
  (bind (Just 4) >>= (lift sqrt))
  (bind (Just 2) >>= (guardf even?) >>= (lift (* 2)))
  (bind (Just 2) >>= (guardf odd?) >>= (lift (* 2)))
  (sum/m '(Nothing Nothing (Just 5)))]

Examples with invalid types:
@interaction[#:eval formica-eval
  (bind 4 >>= (lift sqrt))
  (bind (Just 2) >>= (lift sqrt))]

We may use any type inside Maybe:
@interaction[#:eval formica-eval
(using (Maybe Sym) (map mplus '(x y z) '(y z x)))]

@defproc*[([(monad? [v Any]) Bool]
           [(monad-plus? [v Any]) Bool])]
Return @racket[#t] if @racket[_v] is monad or additive monad, respectively. Otherwise return @racket[#f]. Any additive monad satisfies the @racket[monad?] predicate.

Examples:
@interaction[#:eval formica-eval
  (monad? Id)
  (monad? List)
  (monad-plus? Id)
  (monad-plus? List)]

@section{Switching between monads}

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

@section{Monadic computations}

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
 (using List
   (bind '(1 2 3) >>= (lift sqr)))]

@interaction[#:eval formica-eval
 (using Id 
   (bind 8 >>= displayln >> 5 >>= sqr))
 (using List
   (bind '(1 2 3) >>= (lift displayln) >> '(4 5 6) >>= (lift sqr)))]

When called without subform, @racket[bind] evaluates to a binding function of the @tech{currently used monad}.
@interaction[#:eval formica-eval
 (using List
   (apply bind (list '(a b c) (lift f))))]

@defform/subs[#:literals (<- <- <<- <<-:) (do ops ...+ res) 
([ops (pat <- expr) (pat <-: expr) ((pat1 pat2 ...) <<- expr) ((pat1 pat2 ...) <<-: expr) (expr)])]
Performs sequential computations in the context of the @tech{currently used monad}.
Mimics do-syntax of @emph{Haskell} language.

Operators @racket[_ops] could have any of following forms:
@itemize{
  @item{@racketidfont{(pat @(defidform/inline <-) expr)} matches expression @racket[_expr] with pattern @racket[_pat] and binds named patterns.}
  @item{@racketidfont{(pat @(defidform/inline <-:) expr)} equivalent to @racket[(pat <- (return _expr))].}
  @item{@racketidfont{((pat1 pat2 ...) @(defidform/inline <<-) expr)} equivalent to a sequence @racket[(_pat1 <- _expr) (_pat2 <- _expr) ...].}
  @item{@racketidfont{((pat1 pat2 ...) @(defidform/inline <<-:) expr)} equivalent to a sequence @racket[(_pat1 <-: _expr) (_pat2 <-: _expr) ...].}
  @item{@racket[(_expr)] evaluates @racket[_expr] for side effects or guarding.}}

@bold{Examples:}

@defs+int[#:eval formica-eval
 ((define-formal f))]

Simple monadic binding:
@interaction[#:eval formica-eval
 (using List
   (do [x <- '(1 2 3)]
       [y <- '(a b c)]
       (return (f x y))))]

Using pattern-matching
@interaction[#:eval formica-eval
 (using List
   (do [(cons x y) <- '((1 . 2) (3 . 4))]
       (return (f x y))))]

Here the @racket[x] is binds to a whole list, not to it's elements:
@interaction[#:eval formica-eval
 (using List
   (do [x <-: '(1 2 3)]
       [y <- '(a b c)]
       (return (f x y))))]

Sequential binding:
@interaction[#:eval formica-eval
 (using List
   (do [(x y) <<- '(1 2 3)]
       (return (f x y))))]

@interaction[#:eval formica-eval
 (using List
   (do [(x y) <<-: '(1 2 3)]
       [z <- x]
       (return (f x y z))))]

Guarding
@interaction[#:eval formica-eval
 (using List
   (do [(x y) <<- '(1 2 3)]
       (guard (< x y))
       (return (f x y))))]

@section{Monadic functions and operators}

@defproc[(return [x Any]) any]
The unit function of @tech{currently used monad}.

Examples:
@interaction[#:eval formica-eval
 (using Id (return 5))
 (using List (return 1 2 3))]

@defthing[mzero Any]
A zero element of the @tech{currently used monad}.

Examples:
@interaction[#:eval formica-eval
 (using List mzero)
 (using Id mzero)]


@defproc[(mplus [x Any] [y Any]) Any]
A monadic plus operation of the @tech{currently used monad}.

Examples:
@interaction[#:eval formica-eval
 (using List (mplus '(1 2 3) '(3 4 5)))]