#lang scribble/doc

@(require (for-label formica))

@(require
  scribble/manual
  scribble/eval)

@(define formica-eval
   (let ([sandbox (make-base-eval)])
     (sandbox '(require formica))
     sandbox))

@title[#:tag "comparison"]{Comparison and ordering}

@declare-exporting[formica]

The bindings documented in this section are provided by the @racket[formica/tools] library and @racket[formica] language.

@defproc[(different? [v1 Any] [v2 Any] ...+) Bool]
Returns @racket[#t] if @racket[_v1] and @racket[_v2] are not @racket[equal?], and @racket[#f] otherwise. If more then two arguments are given, they are compared pairwise, so the result is @racket[#t] if none of arguments are @racket[equal?].

Examples:
@interaction[#:eval formica-eval
  (different? 1 1)
  (different? 1 2)
  (different? 1 1.0)
  (different? 1 2 6 3 9 7)
  (different? 1 2 6 3 2 7)]

@defproc[(almost-equal? [v1 Any] [v2 Any] ...+) Bool]
@defthing[≈ almost-equal?]
Returns @racket[#t] if @racket[_v1] and @racket[_v2] are @racket[equal?], or for numeric values if the magnitude of difference between @racket[_x] and @racket[_y] is less then @racket[(tolerance)]. Returns @racket[#f] otherwise.

If lists and pairs are compared, they are @racket[almost-equal?] if they have the same structure and all entries at corresponding positions are @racket[almost-equal?].

If more then two arguments are given, they are compared pairwise from left to right.

Function @racket[almost-equal?] has an alias: @racket[≈]
(could be entered as @litchar{\approx} + @onscreen{Alt} @litchar{\}).

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

@defparam[tolerance x Real]
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

@section{Generic ordering}

Symbolic transformations often require ordering of objects of different kinds: numbers, booleans, strings or lists. For example, these two algebraic expressions, should be equivalent: @centered{@tt{'(+ a b) = '(+ b a).}} As another example, consider following simplification chain:
@centered{@tt{'(+ x 2 (* y x) 5)}   ==> 
          
          @tt{'(+ 2 5 x (* x y))}   ==> 
          
          @tt{'(+ 7 x (* x y)).}}
So it is reasonable to sort the arguments of commutative  operations. One more example is given in the @filepath{logics.rkt} file in the @filepath{formica/examples} folder.

Formica provides a generic ordering procedure which allows to define the ordering on different sets which are represented by contract-based types.

By default following ordering of different types is used:
@tabular[#:sep @hspace[2]
         (list (list @bold{Order} @bold{Type} @bold{Ordering function})
               (list "1"   @racket[#t]    @racket[(const #f)])
               (list "2"   @racket[#f]    @racket[(const #f)])
               (list "3"   @racket[Real]    @racket[<])
               (list "4"   @racket[Str]    @racket[string<?])
               (list "5"   @racket[Sym]    @racket[symbol<?])
               (list "6"   @racket[null?]    @racket[(const #f)])
               (list "7"   @racket[pair?]    @racket[pair<?]))]
It means that any string follows any real number, and for comparing values within the type the corresponding ordering function is used.
This table is stored in the @racket[(type-ordering)] parameter and could be extended or modified.

@defproc[(ordered? [v Any] ...) Bool]
Returns @racket[#t] if arguments @racket[_v ...] are ordered according to ordering given by the @racket[(type-ordering)] parameter. Returns @racket[#f] otherwise.

Examples:
@interaction[#:eval formica-eval
 (ordered? 1)
 (ordered? 'x 'y)
 (ordered? 1 "a" 'x)
 (ordered? 1 "a" 'x #f)
 (sort '(#f 'y (3 2) 2.0 "ab" 'x 'b 3 "abc" 'a "bca" (1 (2 3)) (1 2) 1 (1) #t) ordered?)]

@defparam[type-ordering p (list: (cons: Type (Any Any → Bool)))]
A parameter which defines the ordering of different types and ordering functions within types.

In following example even numbers follow odd numbers, moreover, within odd numbers reverse ordering is set up. Symbols are not ordered, hence they are left unsorted, but shifted to the right.
@interaction[#:eval formica-eval
 (parameterize ([type-ordering (list (cons odd? >)
                                     (cons even? <))])
   (sort '(0 1 2 3 'x 4 5 6 'a 7 8 9) ordered?))]

@defproc[(add-to-type-ordering (type Type) (prec-type (or/c Type 'last 'first) 'last) (ord-fun (Any Any → Bool) (cons #f))) void?]
Adds @racket[_type] to the current @racket[(type-ordering)] table. If the @racket[_prec-type] is given, the new type will have the order next to it. If the comparing function @racket[_ord-fun] is given, it will be used to compare values within the type. If @racket[_prec-type] is equal to @racket['last] or @racket['first] the new type will be appended or, correspondingly prepended to the current @racket[(type-ordering)] table.

If @racket[_type] already exists in the current @racket[(type-ordering)] table, it's ordering will be changed according to new @racket[_prec-type] and @racket[_ord-fun].

In this example complex numbers follow reals and precede strings. Moreover, within complex numbers ordering according to magnitude is set up. Symbols are ordered in default way.
@interaction[#:eval formica-eval
 (parameterize ([type-ordering (type-ordering)])
   (add-to-type-ordering complex? Real (argmap < magnitude))
   (sort '(0-i 2 #t 3.5 1+2i "x" 4-i 5 6 "a" 7 -1+2.5i 9 #f) ordered?))]

In theese examples complex numbers are added at the beginning or at the end of the ordering table.
@interaction[#:eval formica-eval
 (parameterize ([type-ordering (type-ordering)])
   (add-to-type-ordering complex? 'first (argmap < magnitude))
   (sort '(0-i 2 #t 3.5 1+2i "x" 4-i 5 6 "a" 7 -1+2.5i 9 #f) ordered?))
 (parameterize ([type-ordering (type-ordering)])
   (add-to-type-ordering complex? 'last (argmap < magnitude))
   (sort '(0-i 2 #t 3.5 1+2i "x" 4-i 5 6 "a" 7 -1+2.5i 9 #f) ordered?))]

This changes the ordering of booleans:
@interaction[#:eval formica-eval
(parameterize ([type-ordering (type-ordering)])
   (add-to-type-ordering #t #f)
   (sort '(0-i 2 #t 3.5 1+2i "x" 4-i 5 6 "a" 7 -1+2.5i 9 #f) ordered?))]

@defproc[(symbol<? [s1 Sym] [s2 Sym]) Bool]
Returns @racket[#t] if @racket[_s1] precedes @racket[_s2] in lexicographic order, and @racket[#f] otherwise.

Examples:
@interaction[#:eval formica-eval
  (symbol<? 'x 'y)
  (symbol<? 'abcde 'acde)
  (symbol<? 'x 'x)
  (sort '(a X abc x abcd A) symbol<?)]

@defproc[(pair<? [p1 pair?] [p2 pair?]) Bool]
Defines lexicographic order on a set of pairs, according to ordering of pair elements.

Examples:
@interaction[#:eval formica-eval
  (pair<? '(1 2) '(1 2 3))
  (pair<? '(1 2) '(1 3))
  (pair<? '(1 . 2) '(1 2))
  (pair<? '(1 2) '(1 x y))]
