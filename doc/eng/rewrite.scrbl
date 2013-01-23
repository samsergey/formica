#lang scribble/manual

@(require
  scribble/eval
  (for-label formica))

@(define formica-eval
   (let ([sandbox (make-base-eval)])
     (sandbox '(require formica))
     sandbox))


@title[#:style '(toc) #:tag "rewriting"]{The Term Rewriting}

@declare-exporting[formica/rewrite]

The bindings documented in this section are provided by the @racket[formica/rewrite] library and @racket[formica] language.

The Formica provides tools for programming via term rewriting technique. Provided forms could be considered as a syntactic sugar for Racket's @racket[match-lambda] form, however they offer different semantics. The language introduces repetitive rewriting in order to obtain the normal form of given expression, ability to transform any sub-part in nested list structure.

@local-table-of-contents[]

@section{Motivation}

The thoroughly developed rewriting theory has many applications in different fields of computer science @cite["Baader99"  "Bezem03"]. It could be a handy tool in everyday programming practice, making program units shorter and more expressive. The REFAL @cite["Turchin89" "Surhone10"] and Wolfram's Mathematica core language @cite["Wolfram"] give excellent examples of scalability, power and expressiveness provided by rewriting.

The aim of developing this library is to mimic Mathematica's rewriting tools in Scheme. The library provides four forms: @racket[rewrite], @racket[rewrite-all], @racket[rewrite-repeated] and @racket[rewrite-all-repeated] named almost in the same way as rewriting functions in Mathematica. We use word ``rewriting'' for ``replacement'', ``substitution'' or ``transformation'' in this manual, since ``rewriting'' has precise mathematical meaning.

@deftech{Rewriting} is a formal method of replacing  an expression or subterms of an expression with other terms. Here the role of the expression play any datum, lists, structures and @tech{formal applications}.

@subsection{Short examples}

Rewriting is a function:
@interaction[#:eval formica-eval
                    (/. 'a --> 'b)]

It replaces all occurrences of @racket['a] to @racket['b] in a nested list structure:
@interaction[#:eval formica-eval
                    ((/. 'a --> 'b) '(1 a ((a b c) a)))]

This rewriting describes cyclic swapping of symbols:

@interaction[
             #:eval formica-eval
                    ((/. 'a --> 'b 'b --> 'c 'c --> 'a) '(1 a ((a b c) a)))]

Rewriting rules can be used as lambda-functions:

@interaction[
             #:eval formica-eval
                    ((/. x --> (* x x)) 4) ;unary
                    ((/. x y --> (* y x)) 4 5)
                    ((/. x (cons y z) --> (values (* x y) z)) 4 '(1 2 3))]

Rewriting is performed at any level of a list structure:
@interaction[
             #:eval formica-eval
                    ((/. (? Num x) --> (* x x)) '(1 (2 3) 4))]

Rewriting rules accept any pattern recognized by @racket[match] form:
@interaction[
             #:eval formica-eval
                    ((/. (list _ (and x (app length 2)) (or 1 2)) --> x) `(a (1 4) 2))]


Repetitive rewriting could be used to find a fixed point of a function. 
This finds a square root of 2 using Newton's method:

@interaction[
             #:eval formica-eval
                    ((//. x  --> (/ (+ x (/ 2 x)) 2)) 1.)]

Simple implementation of symbolic η- and β-reduction rules for λ-calculus:
@margin-note{We need @racket[eval] here because neither rewriting rules nor replace form (@racket[/.]) are 
                     first class objects (same as @racket[match] and it's patterns).}

@#reader scribble/comment-reader
   (interaction #:eval formica-eval
    (define reduce
      (//. ; η-reduction
           `(λ. ,x (,f ,x)) --> f
           ; β-reduction
           `((λ. ,x ,B) ,A) --> (eval `((/. ',x --> ',A) ',B))))
   )

These rules read: ``The λ-term @math{λ.x (f x)} could be rewritten as @math{f} ''. ``Application of a λ-term @math{((λ . x B) A)} is done by rewriting each occasion of a formal argument @math{x} by @math{A} in the function's body @math{B}''. Now we are able to make symbolic reduction of λ-terms:
@interaction[
             #:eval formica-eval              
                    (reduce '((λ. x (f x x)) a))
                    
                    (reduce '((λ. f (λ. x (f (f x)))) (λ. f (λ. y (f (f y))))))
                    
                    (define ω '(λ. f (λ. x ((f f) x))))
                    
                    (reduce `(,ω (λ. x x)))
                   
                    (reduce `((λ. x y) (,ω ,ω)))]
@margin-note{To be precise, it is not really lazy. If the whole expression were not a β-redex, rewriting would proceed with it's parts and finally fall into @math{(ω ω)} infinite loop.}
The last example shows that we have implemented a sort of lazy evaluation. This is a reflection of the rewriting strategy: first try to rewrite the whole expression and then it's parts.

More examples could be found in the @filepath{formica/examples} folder within the package distribution.
Examples include:
@itemize{
         @item{@filepath{rewrite.rkt} -- more basic examples.}
          @item{@filepath{automata.rkt} -- finite automata defined with rewriting.}
          @item{@filepath{infix.rkt} -- transformation of algebraic expressions given in infix notation, to prefix notation with parenthesis and reverse polish notation.}
          @item{@filepath{peano.rkt} -- definition of Peano axioms.}
          @item{@filepath{logics.rkt} -- simple tautology prover.}
          @item{@filepath{turing.rkt} -- a Turing machine interpreter.}}

@section{Rewriting Forms}

@subsection{Singlefold Rewriting}

@defform/subs[#:literals (--> => ?)(rewrite rule-spec ...) 
([rule-spec (pat ... --> expr) 
(pat ... --> (=> id) expr) 
(pat ... --> (? guard) expr)])]{Transforms to a function which applies rules specified by @racket[rule-spec] in an attempt to transform the entire input expression. If no rules could be applied, input expression is left unchanged.

@racket[pat] --- any pattern suitable for the @racket[match] form. The sequence of patterns correspond to a sequence of arguments to which a rewriting function is applied.

@racket[expr] --- any expression. In contrast to @racket[match] the right-hand side part of the rule should contain only one expression. Use  @racket[begin] for sequencing.

@defexamples[#:eval formica-eval
                (define f 
                  (rewrite x --> (* x x) 
                           x y --> (* x y)))
                (f 4)
                (f 4 5)
                (procedure-arity f)]

If none of patterns match, the input arguments are returned as a list.

An optional @racket[(=> id)] between patterns and the @racket[expr] is bound to a failure procedure of zero arguments. It works in the same way as in the @racket[match] form.

An optional @racket[(? guard)] between patterns and the @racket[expr] is used to guard the rule. The @racket[_guard] is evaluated with bindings given by patterns. If @racket[guard] evaluates to @racket[#f] evaluation process escapes back to the pattern matching expression, and resumes the matching process as if the pattern had failed to match. Otherwise the rule is applied. 

@examples[#:eval formica-eval
                    (define g 
                      (rewrite x y --> (? (< x y)) (* x y)
                               _ y --> y))
                    (g 3 4)
                    (g 4 3)]
}

@defform[(rewrite-all rule-spec ...)]{Transforms to a procedure which applies rules specified by @racket[rule-spec] in an attempt to transform each subpart of the input expression. If no rules could be applied, input expression is left unchanged.}
@defform[(/. rule-spec ...)]{An alias for the @racket[rewrite-all] form.}

@examples[#:eval formica-eval
                    ((/. 'a --> 'x) '(a b a d a))
                    ((/. 'a --> 'x) '(a (b (a) d) a))
                    ((/. 'a --> 'b 'b --> 'a) '(a (b (a) b) a))]

Only unary rules could be applied to subexpressions. 
At the same time multiary rules could be applied to a sequence of input arguments.
@examples[#:eval formica-eval
                (define g 
                  (/. (? Num x) --> (* x x) 
                      x y --> (* x y)))
                (g 4)
                (g '(4 5))
                (g 4 5)]

@defform[(define/. id rule-spec ...)]{Expands to @racket[(define id (/. rule-spec ...))].}

@subsection{Repetitive Rewriting}
Repetitive rewriting rules could be either @emph{regular} or @emph{terminal}.
Application of repetitive rewriting rules follows the algorithm:
@itemize{
         @item{Consequently try to apply given rules to the expression.}
          @item{If a pattern, corresponding to a @emph{regular} rule matches (rule with @(defidform/inline -->) arrow), make rewriting and apply rules to the result, starting from the beginning of the rule sequence.}
          @item{If a pattern, corresponding to a @emph{terminal} rule matches (rule with @(defidform/inline -->.) arrow), make rewriting, stop the rewriting process and return the result.}
          @item{The rewriting process stops either if expression does not change after rewriting, or if the last applied rule was terminal.}}

@defform/subs[#:id rewrite-repeated #:literals (--> -->. => ?) (rewrite-repeated rule-spec ...) 
([rule-spec (pat ... ar expr) 
(pat ... ar (=> id) expr) 
(pat ... ar (? guard) expr)]
[ar --> -->.])]{Transforms to a function which applies rules, specified by @racket[rule-spec], repeatedly in an attempt to get the normal form of the entire input expression.

Arrows  @racket[-->] and  @racket[-->.] correspond to regular and terminal rewriting rules, respectively.
} 

@defform[(rewrite-all-repeated rule-spec ...)]{Transforms to a procedure which repeatedly performs rewriting in every part of the given expression until either result no longer changes, or terminal rule is applied. It performs one complete pass over the expression using @racket[rewrite-all], then carries out the next pass.
Each pass is done using preorder traversal of nested list structure.                                      }    

@defform[(//. rule-spec ...)]{An alias for @racket[rewrite-all-repeated] form.}   

@examples[#:eval formica-eval
                    ((//. 'a --> 'b 
                          'b --> 'c
                          'c --> 'd) '(a b c))]

Using a terminal rule
@interaction[#:eval formica-eval
                    ((//. 'a -->. 'b 
                          'b --> 'c
                          'c --> 'a) '(a b c))]
If rewriting proceeds with multiple values, these values could be accepted as arguments during repeating iterations.

Example. Calculation of GCD:
@interaction[#:eval formica-eval
((//. a 0 -->. a
      a b --> (values b (modulo a b))) 225 125)]

Let's compare three functions which iteratively calculate the @math{n}-th Fibonacci number. The first of them is defined in a regular recursive way, the second implements recursion via rewriting, and the third one uses repetitive rewriting.
@defs+int[#:eval formica-eval
                 [(define (fib1 n)
                    (case n
                      [(1) 0]
                      [(2) 1]
                      [else (let loop ([a 0] [b 1] [i n])
                              (if (= i 3) 
                                (+ a b)
                                (loop b (+ a b) (- i 1))))]))
                  
                  (define fib2
                    (rewrite
                     1 --> 0
                     2 --> 1
                     n --> (fib2 0 1 n)
                     a b 3 --> (+ a b)
                     a b i --> (fib2 b (+ a b) (- i 1))))
                  
                  (define fib3
                    (rewrite-repeated
                     1 -->. 0
                     2 -->. 1
                     n --> (values 0 1 n)
                     a b 3 -->. (+ a b)
                     a b i --> (values b (+ a b) (- i 1))))]
                 
                 (let ([n 10000])
                   (= (time (fib1 n)) (time (fib2 n)) (time (fib3 n))))]

@defform[(define//. id rule-spec ...)]{Expands to @racket[(define id (//. rule-spec ...))].}


             