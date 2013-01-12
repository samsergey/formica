#lang scribble/doc

@(require (for-label formica))

@(require
  scribble/manual
  scribble/eval)

@(define formica-eval
   (let ([sandbox (make-base-eval)])
     (sandbox '(require formica))
     sandbox))

@title{Monads, defined in Formica}

@declare-exporting[formica]

@section{Id}

@defthing[Id monad?] 
The identity monad.

Definition:
@codeblock{return = id
           bind _m _f = (_f _m)}

Examples:
@defs+int[#:eval formica-eval
 ((using-monad Id)
 (define-formal f))
 (return 'x)
 (bind 'x >>= f)]

In the @racket[Id] monad @racket[do] form works like @racket[match-let*] form with ability to produce side effects within calculations:
@interaction[#:eval formica-eval
 (do [x <- 5]
     [y <- 8]
     (displayln x)
     [x <- (+ x y)]
     (list x y))]

@interaction[#:eval formica-eval
 (do [(cons x y) <- '(1 2 3)]
     [(z t) <<- (reverse y)]
     (return (list x y z t)))]

@section{List}

@defthing[List monad-plus?] 
The @racket[List] monad is used for list comprehension and in order to perform calculations with functions, returning more then one value. The main differenece between the @racket[List] and monad @tt{[]} in @emph{Haskell}, is the ability of @racket[List] to operate with any sequences as @racket[for] iterators do.

Definition:
@codeblock{return = list
           bind _m _f = (concat-map _f _m)
           mzero = null
           mplus = concatenate
           type = listable?
           failure = (const null)}

@defproc[(listable? (v Any)) Bool]
Returns @racket[#t] if @racket[v] is a sequence but not the @tech{formal application}.

Examples:
@interaction[#:eval formica-eval
  (listable? '(1 2 3))
  (listable? 4)
  (listable? (stream 1 2 (/ 0)))
  (listable? '(g x y))
  (define-formal g)
  (listable? (g 'x 'y))]

@defproc[(concatenate (s listable?) ...) list?]
Returns a result of @racket[_s ...] concatenation in a form of a list.

Examples:
@interaction[#:eval formica-eval
  (concatenate '(1 2 3) '(a b c))
  (concatenate 4 (stream 'a 'b 'c))
  (concatenate (in-set (set 'x 'y 'z)) (in-value 8))
  (concatenate 1 2 3)]

@defproc[(concat-map (f (any/c -> n/f-list?)) (s listable?)) list?]
Applies @racket[_f] to elements of a @racket[_s] and returns a concatenation of results.

Examples:
@interaction[#:eval formica-eval
  (concat-map (λ (x) (list x (- x))) '(1 2 3))
  (concat-map (λ (x) (list x (- x))) 4)]