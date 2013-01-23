#lang scribble/doc

@(require (for-label formica))

@(require
  scribble/manual
  scribble/eval)

@(define formica-eval
   (let ([sandbox (make-base-eval)])
     (sandbox '(require formica
                        racket/match))
     sandbox))

@title[#:style '(toc) #:tag "formal"]{Formal functions}

@declare-exporting[formica]

The bindings documented in this section are provided by the @racket[formica/formal] library and @racket[formica] language.

@local-table-of-contents[]

The concept of formal functions was taken from symbolic computation languages, such as
@emph{Wolfram Mathematica} or @emph{Maxima}. Together with pattern-matching and rewriting techniques formal
functions give powerful and flexible framework for development of abstract data types,
symbolic computations, debugging of functional programs and teaching different programming concepts.

A @deftech{@emph{formal function}} if applied to any
arguments does not perform any computations and returns a literal expression denoting
it's application (the @deftech{@emph{formal application}}).

@centered[@tt{(f 'x 'y)  ==>  '(f x y)}]

The formal application is just a list, having the name of function at the first position and 
the sequence of arguments as the rest of it.

Formal functions are in many ways similar to Racket's structures: they both provide tagged containers, 
which could be identified and analyzed by pattern-matching and contract systems.
Here are some points which show the difference between formal functions and structures.
@itemize{@item{Formal functions do not give names to their arguments as Racket structures do.}
         @item{Unlike structure formal function may accept different number of arguments,
               as a @tech{variadic} or @tech{polyadic} function. Structures allowed to have only optional fields
               with default values.}
         @item{Because formal applications are lists, they are much more flexible then structures.
               Formal applications could be combined and transformed, mapped and folded as any list. 
               However they could be identified by pattern-matching, as structures.}}

@section[#:tag "formal motivation"]{Motivation}

Pattern matching usually goes together with algebraic types. 
Racket's structures and lists could be used as constructors for algebraic data types, but their use have some drawbacks.

Suppose we wish to define a rewriting system for Peano axioms. We start with definition of types for numerals, successor, sum and product:
@defs+int[#:eval formica-eval
                 [(struct N (x) #:transparent)
                  (struct 1+ (x) #:transparent)
                  (struct Sum (x y) #:transparent)
                  (struct Prod (x y) #:transparent)]]
Next we define rewriting rules:
@def+int[#:eval formica-eval
                (define calculate
                  (rewrite-all-repeated 
                   (N 0) --> 0
                   (N n) --> (1+ (N (- n 1)))
                   
                   (Sum x 0) --> x
                   (Sum x (1+ y)) --> (1+ (Sum x y))
                   
                   (Prod _ 0) --> 0
                   (Prod x (1+ y)) --> (Sum x (Prod x y))))
                (calculate (Sum (N 2) (N 3)))]

However, this clear definition doesn't work, because the rewriting process can't get into the structures and change their parts. One solution is to add rules for penetrating into the structures:
@def+int[#:eval formica-eval
                (define calculate2
                  (rewrite-repeated 
                   (N 0) --> 0
                   (N n) --> (1+ (N (- n 1)))
                   
                   (Sum x 0) --> x
                   (Sum x (1+ y)) --> (1+ (Sum x y))
                   
                   (Prod _ 0) --> 0
                   (Prod x (1+ y)) --> (Sum x (Prod x y))
                   
                   (1+ x) --> (1+ (calculate2 x))
                   (Sum x y) --> (Sum (calculate2 x) (calculate2 y))
                   (Prod x y) --> (Prod (calculate2 x) (calculate2 y))))
                (calculate2 (Sum (N 2) (N 3)))
                (calculate2 (Prod (N 2) (N 3)))]
Now it works! But heavy and tautological lines which end up the rewriting system look cumbersome. They describe programming, not Peano axioms.

Moreover, we need explicit recursion, therefore we have lost the ability to make anonymous rewriting system. Finally, use of structures fixes the number of their fields, so it is impossible to write a pattern like @racket[(Sum x ___)] and to have any number of arguments, which is reasonable when dealing with sums.

Another approach is to use lists and interpret their @racket[car]-s as tags.
@def+int[#:eval formica-eval
                (define calculate3
                  (rewrite-all-repeated 
                   `(N 0) --> 0
                   `(N ,n) --> `(1+ (N ,(- n 1)))
                   
                   `(Sum ,x 0) --> x
                   `(Sum ,x (1+ ,y)) --> `(1+ (Sum ,x ,y))
                   
                   `(Prod ,_ 0) --> 0
                   `(Prod ,x (1+ ,y)) --> `(Sum ,x (Prod ,x ,y))))
                
                (calculate3 '(Sum (N 2) (N 3)))
                (calculate3 '(Prod (N 2) (N 3)))]

Looks much better, but a bit prickly because of quotes and commas. It may become even worse-looking if we use blanks _ and ___ a lot. Using explicit @racket[list] constructor does not help either, it makes patterns more difficult to read and write: @racket[(list 'Sum x (list '1+ y))].

With use of formal functions we may write clear definition of rewriting system for Peano's numerals without a single redundant word or symbol:
@defs+int[#:eval formica-eval
                 [(define-formal N 1+ Sum Prod)
                  (define calculate4
                    (rewrite-all-repeated 
                     (N 0) --> 0
                     (N n) --> (1+ (N (- n 1)))
                     
                     (Sum x 0) --> x
                     (Sum x (1+ y)) --> (1+ (Sum x y))
                     
                     (Prod _ 0) --> 0
                     (Prod x (1+ y)) --> (Sum x (Prod x y))))]
                 
                 (calculate4 (Sum (N 2) (N 3)))
                 (calculate4 (Prod (N 2) (N 3)))]

Besides, formal functions could be useful in general. For example in exploring functional programming:

@interaction[#:eval formica-eval
                    (define-formal f g h)
                    (map f '(a b c))
                    (foldl g 'x0 '(a b c))
                    (foldr g 'x0 '(a b c))
                    ((compose f g h) 'x 'y)]

@section[#:tag "formal creation"]{Creation of formal functions}

@defform/subs[(define-formal f-spec ...) 
([f-spec
   id
   (id arity-spec)])
 #:contracts ([id Sym]
              [arity-spec procedure-arity?])]

Defines @tech{formal functions}, according to specifications @racket[_f-spec ...], where
@racket[_id] is the name of a formal function and @racket[_arity-spec] is it's arity.

For each formal function @racket[_id] the @racket[define-formal] form also defines
@itemize{@item{the predicate @racket[_id]@racketidfont{?}, which returns @racket[#t] for formal application of @racket[_id] and @racket[#f] otherwise;}
         @item{the syntax form @racket[_id]@racketidfont{:}, which could be used as a contract for the formal application with arguments of given types.}}

@examples[#:eval formica-eval
  (define-formal f)
  f
  (f 2)
  (f 'x 'y)
  (map f '(a b c) '(x y z))
  (foldr f 'x0 '(a b c))
  (f? (f 1 2))
  (f? '(1 2))
  (is (f 1) (f: Num))
  (is (f 1 'x) (f: Num Sym))
  (is (f 1 2 3) (f: Num ..))]

Formal function with fixed arity:

@interaction[#:eval formica-eval
  (define-formal (g 2))
  g
  (g 2)
  (g 'x 'y)
  (g 1 2 3)
  (map g '(a b c) '(x y z))]

Using formal applications as patterns:

@interaction[#:eval formica-eval
  (define-formal f (g 2))
  (match (f 5) 
     [(f x) x])
  (match (f 1 2 3 4) 
     [(f x y ...) y])
  (match (g 1 (f 2 3)) 
     [(g a (f b c)) (list a b c)])
  (match (g 1 2) 
    [(g a) a])
  (match (g 1 2) 
    [(g a b c) (list a b c)])]

The last two cases have wrong syntax because @racket[g] was declared to be a binary function.

@deftogether[(
              @defproc[(hold [f (or/c Fun Sym)] [arity procedure-arity? (arity-at-least 0)]) formal-function?]
              @defthing[#:kind "alias" $ hold])]
Returns a @tech{formal function}, named like function @racket[_f] with arity specified by @racket[_arity].
Function @racket[hold] has an alias: @(racket $).  

Examples:
@interaction[#:eval formica-eval
  (hold +)
  (map + '(1 2 3) '(10 20 30))
  (map ($ +) '(1 2 3) '(10 20 30))
  (foldr ($ +) 'x0 '(a b c))]

@interaction[#:eval formica-eval
  ($ 'f 2)
  (($ 'f 2) 1)
  ((($ 'f 2) 1) 2)
  (($ 'f 2) 1 2)
  (($ 'f 2) 1 2 3)]

@defproc[(formal-function? [x Any]) Bool]
Returns @racket[#t], if @racket[_x] is a @tech{formal function}, and @racket[#f] otherwise.

Examples:
@interaction[#:eval formica-eval
  (define-formal f)
  (formal-function? f)
  (formal-function? +)
  (formal-function? (hold +))]

@section[#:tag "formal applications"]{Identification of formal applications}

@defproc[(formal? [x Any]) Bool]
Returns @racket[#t], if @racket[_x] is a @tech{formal application}, and @racket[#f] otherwise.
This predicate distinguishes only applications of formal functions, defined using @racket[define-formal].

Examples:
@interaction[#:eval formica-eval
  (define-formal f)
  (formal? (f 1 2))
  (formal? '(+ 1 2))
  (formal? (($ +) 1 2))]

@defform[(formal patt)]
A pattern which matches any @tech{formal application}. 
It allows to name the head of a formal application, not only arguments.

Examples:
@interaction[#:eval formica-eval
  (define-formal f)
  (match (f 1 2)
     [(formal (F args ...)) `(function: ,F arguments: ,args)])
  (match '(+ 1 2)
     [(formal (F args ...)) `(function: ,F arguments: ,args)])]

@defproc*[[[(n/f-pair? [x Any]) Bool]
           [(n/f-list? [x Any]) Bool]]]
Return @racket[#t], if @racket[_x] is a pair (list) but not a @tech{formal application}, and @racket[#f] otherwise.

Examples:
@interaction[#:eval formica-eval
  (define-formal f)
  (n/f-pair? (f 1 2))
  (n/f-pair? '(F 1 2))
  (n/f-list? (f 1 2))
  (n/f-list? '(F 1 2))]

@defform[(formal-out id ...)]
A provide transformer which for each name of the formal function @racket[_id] provides
the predicate @racket[_id?] and form @racket[_id:].

Examples:
@interaction[#:eval formica-eval
  (module a formica
    (define-formal h)
    (provide (formal-out h)))
  (h 1 2)
  (require 'a)
  (h 1 2)
  (h? (h 1 2))
  #;(is (h 1) (h: Num))
  (formal? (h 'x 'y))
  (match (h 1 2 3 4) 
     [(h x y ...) y])]