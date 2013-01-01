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

@title[#:tag "formal"]{Formal functions}

@defmodule[formica/formal]

The bindings documented in this section are provided by the @racketmodname[formica/formal] and @racketmodname[formica] modules.

The concept of formal functions was taken from symbolic computation languages, such as
@emph{Wolfram Mathematica} or @emph{Maxima}. Together with pattern-matching and rewriting techniques formal
functions give powerfull and flexible framework for development of abstract data types,
symbolic computations, debugging of functional programs and teaching different programming concepts.

@elemtag["t:formal"]{A @emph{formal function}} if applyed to any
arguments does not provide any computations and returns a literal expression denoting
it's application (the @emph{formal application}).

@centered[@tt{(f 'x 'y)  ==>  '(f x y)}]

The formal application is just a list, having the name of function at the first position and 
the sequence of arguments as the rest of it.

Formal functions are in many ways similar to Racket's structures: they provide tagged containers, 
which could be identified and analyzed by pattern-matching and contract systems.
Here are some points whuch show the difference between formal functions and structures.
@itemize{@item{Formal functions do not give names to their arguments as Racket structures do.}
         @item{Unlike structure formal function may accept different number of arguments,
               as a @elemref["variadic"]{variadic} or @elemref["polyadic"]{polyadic} function. Structures allowed to have only optional fields
               with default values.}
         @item{Because formal applications are lists, they are much more flexible then structures.
               Formal applications could be combined and transformed, mapped and folded as any list. 
               However they could be identifyed by pattern-matching, as structures.}}

@section{Creation of formal functions}

@defform/subs[(define-formal f-spec ...) 
([f-spec
   id
   (id arity-spec)])
 #:contracts ([id symbol?]
              [arity-spec procedure-arity?])]

Defines @elemref["formal"]{formal functions}, according to specifications @racket[_f-spec ...], where
@racket[_id] is the name of a formal function and @racket[_arity-spec] is it's arity.

For each formal function @racket[_id] the @racket[define-formal] form also defines
@itemize{@item{the predicate @racket[_id?], which returns
               @racket[#t] for formal application of @racket[_id] and @racket[#f] otherwise;}
         @item{the syntax form @racket[(_id: _args ...)], which could be used as a contract 
               for the formal application with arguments of given types.}}

Examples:
@interaction[#:eval formica-eval
  (define-formal f)
  f
  (f 2)
  (f 'x 'y)
  (map f '(a b c) '(x y z))
  (foldr f 'x0 '(a b c))
  (f? (f 1 2))
  (f? '(1 2))
  #;(is (f 1) (f: Num))
  #;(is (f 1 'x) (f: Num Sym))
  #;(is (f 1 2 3) (f: Num ..))]

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

The last two cases have wrong syntax because @racket[g] was declared
to be a binary function.

@defproc[(hold [f (or/c Fun Sym)] [arity procedure-arity? (arity-at-least 0)]) formal-function?]
Returns a @elemref["formal"]{formal function}, named like function @racket[_f] with arity specifyed by @racket[_arity].

Function @racket[hold] has a short alias: @racket[$].  

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
Returns @racket[#t], if @racket[_x] is a @elemref["formal"]{formal function}, and @racket[#f] otherwise.

Examples:
@interaction[#:eval formica-eval
  (define-formal f)
  (formal-function? f)
  (formal-function? +)
  (formal-function? (hold +))]

@defproc[(formal? [x Any]) Bool]
Returns @racket[#t], if @racket[_x] is a @elemref["formal"]{formal application}, and @racket[#f] otherwise.
This predicate distinguishes only applications of formal functions, defined using @racket[define-formal].

Examples:
@interaction[#:eval formica-eval
  (define-formal f)
  (formal? (f 1 2))
  (formal? '(+ 1 2))
  (formal? (($ +) 1 2))]

@defform[(formal patt)]
A pattern which matches any @elemref["formal"]{formal application}. 
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
Return @racket[#t], if @racket[_x] is a pair (list) but not a @elemref["formal"]{formal application}, and @racket[#f] otherwise.

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