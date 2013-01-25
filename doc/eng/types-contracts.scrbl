#lang scribble/doc

@(require (for-label formica))

@(require
  scribble/manual
  scribble/eval)

@(define formica-eval
   (let ([sandbox (make-base-eval)])
     (sandbox '(require formica))
     sandbox))

@title[#:tag "types:contracts"]{Contracts}

All types are defined by @emph{contracts}.
A role of a contract could play
@itemize{@item{a constant which belongs to a @tech{primitive type};}
         @item{an unary predicate, describing the type;}
         @item{a compound contract, constructed by @tech{contract combinators}.}}

@defproc[(Type [v Any]) Bool]
Returns @racket[#t] if @racket[_v] could be used as a contract and @racket[#f] otherwise.

Any constant which belongs to a @tech{primitive type} could be used as a contract, representing a @emph{unit type}:
@interaction[#:eval formica-eval
 (is 5 Type)
 (is 'abc Type)]

Any predicate could be used as a contract:
@interaction[#:eval formica-eval
 (is Num Type)
 (is procedure-arity? Type)
 (is cons Type)]

Contracts could be constructed using @tech{contract combinators}:
@interaction[#:eval formica-eval 
 (Type (and/c integer? positive?))
 (Type (or/c Num (cons: Num Num)))]

@defform[(is v type-pred) #:contracts ([v Any] [t Type])]
Provides safe type check. Returns @racket[#t] if @racket[_v] belongs to a type, defined by the contract @racket[_t], and @racket[#f] otherwise.

This form differs from direct contract application in following:
@itemize{@item{it allows to consider @tech{primitive types};} 
         @item{if application of @racket[_t] leads to exception, the @racket[is] form does not stop running the program and returns @racket[#f].}}

@interaction[#:eval formica-eval
 (is 'abc Sym)
 (is 'abc 'abc)
 (is 1.2 odd?)]

Direct application of @racket[odd?] predicate to non-integer value leads to an error:
@interaction[#:eval formica-eval
 (odd? 1.2)]

Any value may belong to unlimited number of types.
For example number @racket[5] belongs to:
@itemize{@item{the unit type @racket[5]:
           @interaction[#:eval formica-eval (is 5 5)]}          
         @item{the numeric type:
           @interaction[#:eval formica-eval (is 5 Num)]}
         @item{the integer number type:
           @interaction[#:eval formica-eval (is 5 integer?)]}
         @item{the numeric type "odd number":
           @interaction[#:eval formica-eval (is 5 odd?)]}
         @item{algebraic type:
           @interaction[#:eval formica-eval (is 5 (or/c 0 1 2 3 5 8 13))] and so on.}}