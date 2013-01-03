#lang scribble/doc

@(require (for-label formica))

@(require
  scribble/manual
  scribble/eval)

@(define formica-eval
   (let ([sandbox (make-base-eval)])
     (sandbox '(require formica))
     sandbox))

@title[#:tag "types:primitive"]{Primitive data types}

A value belongs to a @deftech{primitive type} if it belongs either to
@itemize{@item{a data type, defined by corresponding predicate (@racket[boolean?],
         @racket[number?], @racket[real?], @racket[integer?],
         @racket[positive?], @racket[string?], @racket[symbol?] etc.),}
         @item{or to a @emph{functional type}.}}

All functions and rewriting systems satisfy the @racket[function?] predicate.

A value belongs to a @deftech{functional type} if it belongs to
@itemize{@item{memoized functions, defined by @racket[memoized?] predicate;}
         @item{curried or partially applied functions, defined by @racket[curried?] predicate;} 
         @item{@tech{formal functions}, defined by @racket[formal-function?] predicate;}
         @item{contracts, defined by @racket[contract?] predicate.}}

Functional types which specify types for arguments and result of a function 
could be declared using @tech{function signatures}.

@section{Predefined primitive types}

Some frequently used primitive types have short names, which denote sets, defined by predicates. They could be used in type definitions, signatures and contracts.

@defthing[Bool contract?] defines a set of boolean values. Equivalent to @racket[boolean?].
@defthing[Num contract?] defines a set of numeric values. Equivalent to @racket[number?].
@defthing[Real contract?]  defines a set of real numbers. Equivalent to @racket[real?].
@defthing[Int contract?]  defines a set of integer numbers. Equivalent to @racket[integer?].
@defthing[Nat contract?]  defines a set of natural numbers.
@defthing[Index contract?]  defines a set of positive integer exceedin zero.
@defthing[Str contract?]  defines a set of strings. Equivalent to @racket[string?].
@defthing[Sym contract?] defines a set of symbols. Equivalent to @racket[symbol?].
@defthing[Fun contract?]  defines a set of functions. Equivalent to @racket[function?].