#lang scribble/doc

@(require (for-label formica))

@(require
  scribble/manual
  scribble/eval)

@(define formica-eval
   (let ([sandbox (make-base-eval)])
     (sandbox '(require formica))
     sandbox))

@title{Introduction}

The @emph{Formica} language was created for educational purpose while teaching the "Functional and logic programming" undergraduate course in the Kamchatka State Technical University (Russia).
The main goal of designing @emph{Formica} is to have a functional programming language as flexible as @emph{Racket} or @emph{Wolfram Mathematica}, and almost as syntactically clean as Mark Tarver's @emph{Qi} or @emph{Haskell}. Being a dialect of @emph{Racket} it should complement the parent language and make it possible to use any of @emph{Racket}'s native libraries.

For student's practical work the @emph{Racket} language was chosen for the following reasons. @emph{Racket} is elegant as @emph{Scheme} and goes with educationally oriented IDE, which makes newcomers feel comfortable. It has active community, rich libraries and provides a lot of real-life instruments for GUI development, web tools etc. Finally, @emph{Racket} is extremely flexible: it encourages creating domain-oriented languages and dialects, so it is natural to come to a new language, created specially for the coursework.

Even though it is mainly educational language, some of it's features (such as @tech{formal functions}, @tech{rewriting}, etc.) could be used in various practical applications.

This guide is addressed to a reader which is familiar both to a @emph{Racket} and to functional programming concepts and presents only @emph{Formica}-specific aspects.

@section{Brief tour}
Here are the main features of the Formica dialect.

@subsection{Formal functions}

This declares @racketidfont{f} to be a @tech{formal function}: a function which doesn't perform any calculations, but just shows it's application.
@def+int[#:eval formica-eval
(define-formal f)
(f 'x)
(f 1 2 3)]

Using the formal function we can see how some high-order functions work:
@interaction[#:eval formica-eval
(map f '(a b c))
(foldr f 'x0 '(a b c))
(foldl f 'x0 '(a b c))]

In the following example the `+` function is made formal:
@interaction[#:eval formica-eval
(foldr (hold +) 0 '(1 2 3))]

Together with pattern-matching and @tech{rewriting} technique, formal functions give a framework for designing complex abstract data types.

@subsection{Rewriting}
  
Suppose we have to define a function that, if it receives 1 returns 0 and if it returns 0 returns 1. If all rules fail the argument is left unchanged:
@def+int[#:eval formica-eval
(define r 
  (rewrite 1 --> 0
           0 --> 1))
(r 0)
(r 1)
(r 'x)
(r '(0 1))]

This function rewrites @racket[1] to @racket[2], @racket[3] to @racket[4], and @racket[4] to @racket[1] anywhere in a list.
@def+int[#:eval formica-eval
(define r 
  (/. 1 --> 2
      3 --> 4
      4 --> 1))
(r '(1 2 (3 4)))]

Following rewrites @racket[1] to @racket[2], @racket[3] to @racket[4], and @racket[4] to @racket[1] repeatedly, until result stops changing. This rewriting system has a normal form: @racket[2].
@def+int[#:eval formica-eval
(define r 
  (//. 1 --> 2
       3 --> 4
       4 --> 1))

(r '(1 2 (3 4)))]

A rewriting-based definition of the @racket[map] function:
@def+int[#:eval formica-eval
  (define/. map
    _ '()        --> '()
    f (cons h t) --> (cons (f h) (map f t)))
 (map ($ 'f) '(a b c))]

Here is a simple implementation of symbolic η- and β-reduction rules for λ-calculus:
@#reader scribble/comment-reader
   (interaction #:eval formica-eval
    (define//. reduce
      ; η-reduction
      `(λ. ,x (,f ,x)) --> f
      ; β-reduction
      `((λ. ,x ,B) ,A) --> (eval `((/. ',x --> ',A) ',B))))
   
@interaction[#:eval formica-eval              
 (reduce '((λ. x (f x x)) a))                
 (reduce '((λ. f (λ. x (f (f x)))) (λ. f (λ. y (f (f y))))))]
  
@subsection{Simplified syntax for partial application and point-free definitions}
  
Once you have written in @emph{Haskell} or on the blackboard:
@codeblock{
 map _f = fold (cons . _f) []
 
 map (* 2) [1 2 3]}
                   
it is difficult not to try it in @emph{Formica}:

@defs+int[#:eval formica-eval
  ((define/c (map f) (foldr (∘ cons f) '())))
  (map (* 2) '(1 2 3))
  (map (map (* 2)) '((1 2) (3 4) (5)))]


@subsection{Contract-based dynamical typing system}

Being not so strict as in @emph{Typed Racket} or @emph{Haskell}, the contract typing system gives a flavor of rich type systems used in functional programming, including abstract, algebraic and polymorphic types.

@def+int[#:eval formica-eval
 (define-type Int-or-X
   integer?
   'X)
 (is 5 Int-or-X)
 (is 'X Int-or-X)
 (is 'x Int-or-X)]

@def+int[#:eval formica-eval
 (:: map ((a -> b) (list: a ..) -> (list: b ..))
   (define/c (map f) 
     (/. (cons h t) --> (cons (f h) (map f t)))))
 (map (* 2) '(1 21 4 3))
 (map cons '(1 21 4 3))]

  
 Building abstract algebraic types with formal functions:

A formal constructor of a pair:
@def+int[#:eval formica-eval
(define-formal kons)
(kons 1 (kons 2 3))]

A constructor of the klist:
@def+int[#:eval formica-eval
 (define (klist . x)
   (foldr kons 'knull x))
 (klist 'a 'b 'c)]

A recursive type declaration:
@def+int[#:eval formica-eval
(define-type klist?
  'knull
  (kons: Any klist?))
 (is (klist 1 2 3) klist?)
 (is (list 1 2 3) klist?)
 (is (kons 1 (kons 2 3)) klist?)]

Some functions to operate with klists:
@defs+int[#:eval formica-eval
 ((define/c (kfold f x0)
    (/. 'knull --> x0
        (kons h t) --> (f h (kfold f x0 t))))

  (define/c (kfilter p) (kfold (fif (∘ p I1) kons I2) 'knull))
  
  (define total (kfold + 0)))

(kfold ($ 'f) 'x0 (klist 'x 'y 'z))

(kfilter odd? (klist 1 2 3 4 5))

(total (klist 1 2 3))]

@subsection{Monads}

Monads give a very useful and elegant abstraction for sequential computations, allowing one to go deep into semantics, keeping syntax simple. Even though @emph{Racket} doesn't need monads to perform sequential computations, side effects or set comprehension, it is useful to have monads as a powerful tool for designing new semantic constructions.

An example of sequential computations in the @racket[List] monad:
@interaction[#:eval formica-eval
(do [x <- '(1 2 3)]
    [x <- (return (+ x 6))]
    (return x))]

A simple list comprehension:
@interaction[#:eval formica-eval
(collect (sqr x) [x <- (in-range 6)] (odd? x))]

Monads allow to define generic functions and then to use them with different semantics. Here is a function returning a sequence of Pythagorean triples:
@def+int[#:eval formica-eval
 (define (find-triples r)
   (collect (list a b c) 
     [a <- r]
     [b <- (in-range 1 a)]
     [c <-: (sqrt (- (sqr a) (sqr b)))]
     (integer? c)
     (> b c)))]

One may use @racket[find-triples] in eager @racket[List] monad, or in lazy @racket[Stream] monad, without changing a single line in the function definition.
@interaction[#:eval formica-eval
(using List (find-triples (in-range 20)))]

@def+int[#:eval formica-eval
 (define t
   (using Stream (find-triples (in-naturals))))
 (stream-take t 5)
 (stream-ref t 50)]

As in @racket[Haskell], it is possible to use pattern-matching in list-comprehensions
@def+int[#:eval formica-eval
 (define primitive-triples
   (using Stream 
     (collect t 
       [(and t (list a b _)) <- (find-triples (in-naturals))] 
       [1 <-: (gcd a b)])))
 (stream-take primitive-triples 5)]

Consider a classical problem: @emph{A farmer buys 100 animals for $100.00. The animals include at least one cow, one pig, and one chicken, but no other kind. If a cow costs $10.00, a pig costs $3.00, and a chicken costs $0.50, how many of each did he buy?}

Here is a declarative program solving this problem:
@def+int[#:eval formica-eval
 (define (solve-farmer-problem #:cow (c 10) 
                               #:pig (p 3) 
                               #:chicken (ch 1/2))
   (collect `((,cows cows) (,pigs pigs) (,chickens chickens))
     [cows     <-  (in-range 1 (/ 100 c))] 
     [pigs     <-  (in-range 1 (/ 100 p))] 
     [chickens <-: (- 100 cows pigs)]
     [= 100 (+ (* c cows) (* p pigs) (* ch chickens))]))
 (solve-farmer-problem)]

If cow costs $8 there would be 6 possible solutions:
@interaction[#:eval formica-eval
 (solve-farmer-problem #:cow 8)]

It is easy to create new monads. Here is an example of defining the parameterized monad @tt{(Maybe a)}.

@defs+int[#:eval formica-eval
  ((define-formal Just)
   
   (define-type (Maybe? a) 
     (Just: a) 
     'Nothing)
   
   (define (Maybe a)
     (monad
      #:type (Maybe? a)
      #:return (/. 'Nothing --> 'Nothing
                    x       --> (Just x))
      #:bind (/. 'Nothing f --> 'Nothing
                 (Just x) f --> (f x))
      #:mzero 'Nothing
      #:mplus (/. 'Nothing x --> x
                   x       _ --> x))))
  (using (Maybe Int)
    (bind (Just 2) >>= (lift sqr) >>= (lift (* 2))))
  (using (Maybe Int)
    (bind 'Nothing >>= (lift sqr) >>= (lift (* 2))))]

This monad could be used to perform guarded computations:
@def+int[#:eval formica-eval
 (define (safe-sqrt x)
   (bind (return x) >>= 
         (guardf positive?) >>= 
         (lift sqrt)))
 (using (Maybe Real)
   (map safe-sqrt  '(-1 4 3 9 -4 -9)))
 (using (Maybe Int) (safe-sqrt 3))]

@emph{Formica} provides monadic functions and operators, such as @racket[lift/m], @racket[compose/m], @racket[fold/m], @racket[map/m], etc.
@def+int[#:eval formica-eval
 (using-monad List)
 (lift/m or '(#t #f) '(#t #f))
 (fold/m (λ (x y) `((+ ,x ,y) (- ,x ,y))) 0 '(1 2 3))]

@def+int[#:eval formica-eval
 (define powerset (filter/m (const '(#t #f))))
 (powerset '(1 2 3))]

Monads could be created through higher order abstractions:
@def+int[#:eval formica-eval
 (define-monad Set
   (Monoid
    #:return set
    #:mplus set-union))
 (using Set
   (lift/m + '(1 2 3) '(2 3 4)))]

@defs+int[#:eval formica-eval
 ((define-monad String
   (Monoid
    #:return string
    #:mplus (flipped string-append))))
 (using String
  (collect (char-upcase x)
    [x <- "abc12xy"] 
    (char-alphabetic? x)
    [x <- (format "~a(~a)" x (char->integer x))]))]


@subsection{Miscellaneous functional tools}

Formica is informally called to be "functionally-oriented", meaning that it provides a lot of tools to operate with functions.


This defines binary and unary @tech{formal functions}:
@def+int[#:eval formica-eval
(define-formal (b 2) (u 1))]

Here is a generalized composition of them:
@interaction[#:eval formica-eval
((∘ b u) 1 2)
((∘ u b) 1 2)]

The generalized composition is associative and has identity function as a neutral element:
@interaction[#:eval formica-eval
(equal? ((∘ (∘ b u) b) 'a 'b 'c)
        ((∘ b (∘ u b)) 'a 'b 'c)
        (b (u (b 'a 'b)) 'c))
(equal? ((∘ id b) 1 2)
        ((∘ b id) 1 2)
        (b 1 2))]

Some purely functional definitions
@def+int[#:eval formica-eval
 (define max (foldr (fif < I2 I1) -inf.0)) 
 (max '(2 3 1 2 3 2 5 3 2))]
@def+int[#:eval formica-eval
 (define/c (all-elements p) (foldr (∘ and p) #f))
 (all-elements odd? '(2 4 5 3 7 8))
 (all-elements even? '(2 4 6 0 12 8))]
@defs+int[#:eval formica-eval 
 ((define all-arguments (/@ and))
  (define complex<? (andf (all-arguments complex?)
                          (/@ < magnitude))))
(complex<? 0-i 2 4+i)
(complex<? 2 1-i)
(complex<? 2 'x 1-i)]

See @filepath{examples/} folder in the @emph{Formica} distribution for more examples. 