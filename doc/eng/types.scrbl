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

Formica has @emph{strict dynamic type system}, with type checking being done at
runtime.

Types are used for identification and guarding only. There is no optimisation based on types as in Typed Racket. It is not necessary to declare types of functions and arguments, they are used in the same way as Racket contracts.

Contract-based type system is implemented in Formica for educational purpose: in order to give students a gentle but quite deep introduction to type systems used in functional programming languages. In Formica it is possible to declare and use abstract algebraic, inductive, parameterized and functional types, still being able to utilize @tech{rewriting}, simplified syntax for @tech{partial application} and definitions in @tech{point-free notation}. Moreower the Formica types have close relation to a concept of @tech{formal functions} widely used in the language.

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