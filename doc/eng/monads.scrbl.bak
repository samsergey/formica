#lang scribble/doc

@(require (for-label formica))

@(require
  scribble/manual
  scribble/eval)

@(define formica-eval
   (let ([sandbox (make-base-eval)])
     (sandbox '(require formica))
     sandbox))

@title[#:tag "monads"]{Monads}

@declare-exporting[formica]

The bindings documented in this section are provided by the @racket[formica/monads] library and @racket[formica] language.

Monads give very elegant and powerful abstraction. In different forms monads exist in any programming language, for example the @racket[let*] form could be considered as limited representation of the @racketidfont{Identity} monad, and @racket[for*/list] iterator provides core functionality of the List monad.
Compare
@tabular[
  (list 
   (list @centered{@bold{Racket}} @centered{@bold{Haskell}}) 
   (list 
    @(racketblock 
      (let* ([x 5]
             [x (+ x 3)])
        (f x))) 
    @(racketblock 
      do x <- 5
         x <- x + 3
         f x))
   '("" "")
   (list 
    @(racketblock 
      (for*/list ([x '(1 2 3 6 9)]
                  [y (in-range x)]
                  #:when (even? y))
        (+ x y))) 
    @(racketblock 
      do x <- [1, 2, 3, 6, 9]
         y <- [0 .. x]
         guard y % 2 = 0
         return (f + x))))]

The possibility to define and use arbitrary monads allows to separate semantics and syntax, according to the wellknown Alan Perlis' principle of "a hundred tools for one data structure". 
In Racket there are 28 different @racket[for] iterators and creating an iterator with new semantics requires non-trivial syntax engeneering (according to examples in the Racket Refference Guide).
The syntax tools for monads are limited by binding (@racket[>>=]) and sequential binding (@racket[do] and @racket[collect]) forms, which could be used in uniform way with @emph{any} monad. Moreover, for @emph{any} monad it is possible to use lifting and composition (@racket[lift], @racket[lift/m], @racket[compose/m]), folding (@racket[fold/m], @racket[map/m], @racket[filter/m]) and guarding (for additive monads). The semantics of all these forms and functions is completely defined only by setting the @racket[return] and @racket[bind] functions (complemented by @racket[mplus] and @racket[mzero] in case additive monads). 

Thus, even though the Racket language does not need monads for imperative programming or list comprehension, as Haskell do, monads provide useful and powerful concept for program design.

@local-table-of-contents[]

@include-section["monads-base.scrbl"]
@include-section["monads-List.scrbl"]