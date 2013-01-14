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

The Formica dialect was created for educational purposes while teaching the "Functional and logical programming" undergraduate course in the Kamchatka State Technical University (Russia).

The main goal of designing the Formica language is to have a functional programming language as flexible as @emph{Racket} or @emph{Wolfram Mathematica}, and almost as syntactically clean as @emph{Qi} or @emph{Haskell}.

Even though it is mainly educational language, some of it's features (such as @tech{formal functions}, @tech{rewriting}, etc.) could be used in various practical applications.

Here are the main features of the Formica dialect.

@section{Formal functions:}

This declares @racketidfont{f} to be a formal function:
@interaction[#:eval formica-eval
(define-formal f)]

Now we can see how some high-order functions work:
@interaction[#:eval formica-eval
(map f '(a b c))
(foldr f 'x0 '(a b c))
(foldl f 'x0 '(a b c))]

In the following example the `+` function is made formal:
@interaction[#:eval formica-eval
(foldr (hold +) 0 '(1 2 3))]


@section{Rewriting:}
  
This rewrites 1 to 2, 3 to 4, and 4 to 1.
@def+int[#:eval formica-eval
(define r 
  (/. 1 --> 2
      3 --> 4
      4 --> 1))
(r '(1 2 3 4))]

This rewrites 1 to 2, 3 to 4, and 4 to 1 repeatedly. The first rule is terminal (the rewriting process stops here).
@def+int[#:eval formica-eval
(define r 
  (//. 1 -->. 2
       3 --> 4
       4 --> 1))

(r '(1 2 3 4))]

This is a rewriting-based definition of the `map` function:
@def+int[#:eval formica-eval
  (define map
    (/. f (cons h t) --> (cons (f h) (map f t))
        _ '()        --> '()))
 (map f '(a b c))]
  
@section{Simplified syntax for partial application and point-free definitions:}
  
Once you have written in Haskell on or the blackboard:
@codeblock{
 fold f x0 = F where F []     = x0
                     F (x:xs) = f h (fold f x0 t)
 
 map f = fold (cons . f) []
 
 map (* 2) [1 2 3]}
                   
it is difficult not to try it it Racket:

@defs+int[#:eval formica-eval
  ((define/c (fold f x0) (/. '()        --> x0
                               (cons h t) --> (f h (fold f x0 t))))
   (define/c (map f) (fold (∘ cons f) '())))
  (map (* 2) '(1 2 3))
  (map (map (* 2)) '((1 2) (3 4) (5)))]


@section{Contract-based dynamical typing system:}

Not so strict as in Typed Racket or Haskell, the contract typing system gives a flavor of rich type systems used in functional programming, including abstract, algebraic and polymorphic types.

@def+int[#:eval formica-eval
 (define-type Int-or-X
   'X
   integer?)
 (is 5 Int-or-X)
 (is 'X Int-or-X)
 (is 'x Int-or-X)]

@def+int[#:eval formica-eval
 (:: add1 (Nat -> Nat)
   (define add1 (+ 1)))
 (add1 5)
 (add1 2.3)]

@def+int[#:eval formica-eval
 (:: map ((a -> b) (list: a ..) -> (list: b ..))
   (define/c (map f) 
     (/. (cons h t) --> (cons (f h) (map f t)))))
 (map (* 2) '(1 21 4 3))]

  
 Building abstract algebraic types with formal functions:

A formal constructor of a pair
@def+int[#:eval formica-eval
(define-formal kons)
(kons 1 (kons 2 3))]

A constructor of the klist
@def+int[#:eval formica-eval
 (define (klist . x)
   (foldr kons 'knull x))
 (klist 'a 'b 'c)]

A recursive type declaration
@def+int[#:eval formica-eval
(define-type klist?
  'knull
  (kons: Any klist?))
 (is (klist 1 2 3) klist?)
 (is (list 1 2 3) klist?)
 (is (kons 1 (kons 2 3)) klist?)]

some functions to operate with klists
@defs+int[#:eval formica-eval
 ((define/c (kfold f x0)
    (/. 'knull --> x0
        (kons h t) --> (f h (kfold f x0 t))))

  (define/c (kfilter p) (kfold (fif (∘ p I1) kons I2) 'knull))
  (define total (kfold + 0)))

(kfold ($ 'f) 'x0 (klist 'x 'y 'z))
(kfilter odd? (klist 1 2 3 4 5))
(total (klist 1 2 3))]

  @section{Monads}

@interaction[#:eval formica-eval
(do [x <- '(1 2 3)]
    [x <- (return (+ x 6))]
    (return x))

(collect (sqr x) [x <- 6] (odd? x))]

Define generic functions

@def+int[#:eval formica-eval
 (define (find-triples r)
   (collect (list a b c) 
     [a <- r]
     [b <- (in-range (ceiling (/ a 2)) a)]
     [c <-: (sqrt (- (sqr a) (sqr b)))]
     (integer? c)
     (> b c)))]

Use eager @racket[List] monad:
@interaction[#:eval formica-eval
(using List (find-triples (in-range 20)))]

Or lazy @racket[Stream] monad
@def+int[#:eval formica-eval
 (define t
   (using Stream (find-triples (in-naturals))))
 (stream-take t 5)
 (stream-ref t 50)]

Create new monads easily:

@defs+int[#:eval formica-eval
  ((define-formal Maybe Just)
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
  
@def+int[#:eval formica-eval
 (define (safe-sqrt x)
   (bind (return x) >>= 
         (guardf positive?) >>= 
         (lift sqrt)))
 (using (Maybe Real)
   (map safe-sqrt  '(-1 4 3 9 -4 -9)))
 (using (Maybe Int) (safe-sqrt 3))]

Use monadic functions and operators
@interaction[#:eval formica-eval
 (using-monad List)
 (define powerset (filter/m (const '(#t #f))))
 (powerset '(1 2 3))]


 @section{Miscellaneous functional tools}

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
 ((define all-arguments (-< and))
  (define complex<? (andf (all-arguments complex?)
                          (-< < magnitude))))
(complex<? 0-i 2 4+i)
(complex<? 2 1-i)
(complex<? 2 'x 1-i)]