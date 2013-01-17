#lang scribble/doc

@(require (for-label formica))

@(require
  scribble/manual
  scribble/eval)

@(define formica-eval
   (let ([sandbox (make-base-eval)])
     (sandbox '(require formica))
     sandbox))

@title[#:style '(toc) #:tag "types"]{The contract-based type system}

@declare-exporting[formica/types]

The bindings documented in this section are provided by the @racket[formica/types] library and @racket[formica] language.

Formica has @emph{strict dynamic type system}, based on cononcept of contracts @cite["Findler" "Krishnamurthi"]. 

Types are used for identification and guarding only. There is no optimisation based on types as in Typed Racket. Type declarations are optional, they are used in the same way as Racket contracts, type checking is done at run-time.

Contract-based type system is implemented in Formica for educational purpose: in order to give students a gentle but quite deep introduction to type systems used in functional programming languages. In Formica it is possible to declare and use abstract algebraic, inductive, parametrized and functional types, still being able to utilize @tech{rewriting}, simplified syntax for @tech{partial application} and definitions in @tech{point-free notation}. Moreover the Formica types have close relation to a concept of @tech{formal functions} widely used in the language.

The type in Formica is bound to a value, not to a variable, it represents a set to which value belongs. Such a set could be specified by enumeration, a predicate function or by the inductive definition.

In Formica any type can be either
@itemize{@item{a @tech{primitive type},}
         @item{an @tech{algebraic type},}
         @item{a @tech{functional type} or a @tech{function signature}.}}

The type checking could be done by conditional forms: @racket[if], @racket[cond], @racket[when] etc., or using @tech{function signature}.

@local-table-of-contents[]

@include-section{types-contracts.scrbl}

@include-section{types-primitive.scrbl}

@include-section{types-construction.scrbl}

@include-section{types-signatures.scrbl}