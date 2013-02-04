Formica
=======

A "functionally-oriented" Racket dialect.

Introduction
------------

The Formica dialect was created for educational purpose while teaching the "Functional and logic programming" undergraduate course in the Kamchatka State Technical University (Russia).

Formica is based on the [Racket programming language](http://planet.racket-lang.org/), famous by it's educational and practical use, and could be considered as a Racket teachpack.


Why Racket?
-----------

1. Racket is elegant, transparent and flexible: it is possible to build from scratch and show any of programming concepts and paradigms, such as abstract data types and type systems, objects, continuations, monads, concurrency; logic, concatenative, reactive, combinatorial programming and so on.
2. The symbols are first-class objects, so one may see how the program works.
3. It is very easy to create a domain specific dialect or a language.
4. Racket has brilliant IDE DrRacket which is very friendly for newcommers.
5. Racket has active community, rich libraries and provides a lot of real-life instruments for GUI development, web tools etc. It is possible to give short and *really* interesting examples during the course: parsers, translators, web-servers, symbolic computations, elements of AI etc.

Why Formica?
------------

The main goal of designing Formica is to have a functional programming language as flexible as Racket or Wolfram Mathematica, and almost as syntactically clean as Mark Tarver’s Qi or Haskell. Being a dialect of Racket it should complement the parent language and make it possible to use any of Racket’s native libraries.

Even though Formica is mainly educational language, some of it's features (such as formal functions, abstract rewriting systems etc.) could be used in various practical applications.

What is inside?
---------------

Formica provides
 
1. a concept of **formal functions** and **abstract rewriting systems**,
2. simplified syntax for **partial applications** and **tacit notation**,
3. handy tools to operate with **monads**,
4. easy to use contract-based type system,
5. a lot of functional programming tools: memoization, generalized composition, combinators and functionals. 

Here are the main features of the Formica dialect.

Formal functions
----------------

This declares f to be a formal function: a function which doesn’t perform any calculations, but just shows it’s application.

```Scheme
(define-formal f)

(f 'x)     ==>  '(f x)
(f 1 2 3)  ==>  '(f 1 2 3)
```

Using the formal function we can see how some high-order functions work:

```Scheme
(map f '(a b c))        ==> '((f a) (f b) (f c))
(foldr f 'x0 '(a b c))  ==> '(f a (f b (f c x0)))
(foldl f 'x0 '(a b c))  ==> '(f c (f b (f a x0)))
```

In the following example the ‘+‘ function is made formal:

```Scheme
(foldr (hold +) 0 '(1 2 3))  ==>  '(+ 1 (+ 2 (+ 3 0)))
```

Together with pattern-matching and rewriting technique, formal functions give a framework for designing complex abstract data types.

Rewriting
---------

Suppose we have to define a function that, if it receives 1 returns 0 and if it returns 0 returns 1. If all rules fail the argument is left unchanged:

```Scheme
(define r
  (rewrite 1 --> 0
           0 --> 1))

(r 0)      ==> 1
(r 1)      ==> 0
(r 'x)     ==> 'x
(r '(0 1)) ==> '(0 1)
```

This function rewrites 1 to 2, 3 to 4, and 4 to 1 anywhere in a list.
```Scheme
(define r
  (/. 1 --> 2
      3 --> 4
      4 --> 1))
 
(r '(1 2 (3 4)))  ==>  '(2 2 (4 1))
```

Following rewrites 1 to 2, 3 to 4, and 4 to 1 repeatedly, until result stops changing. This rewriting system has a normal form: 2.

```Scheme
(define r
  (//. 1 --> 2
       3 --> 4
       4 --> 1))
 
(r '(1 2 (3 4)))  ==>  '(2 2 (2 2))
```

A rewriting-based definition of the `map` function:

```Scheme
(define/. map
  _ '()        --> '()
  f (cons h t) --> (cons (f h) (map f t)))
 
(map ($ 'f) '(a b c))  ==>  '((f a) (f b) (f c))
```

Here is a simple implementation of symbolic η- and β-reduction rules for λ-calculus:

```Scheme
(define//. reduce
  ; η-reduction
  `(λ. ,x (,f ,x)) --> f
  ; β-reduction
  `((λ. ,x ,B) ,A) --> (eval `((/. ',x --> ',A) ',B)))

(reduce '((λ. x (f x x)) a))       ==>  '(f a a)
(reduce '((λ. f (λ. x (f (f x)))) (λ. f (λ. y (f (f y)))))) 
==>
'(λ. x (λ. y (x (x (x (x y))))))
```

Simplified syntax for partial application and point-free definitions
--------------------------------------------------------------------

Once you have written in *Haskell* or on the blackboard:

```Haskell
map f = fold (cons . f) []
map (* 2) [1, 2, 3]
```

it is difficult not to try it in *Formica*:

```Scheme
(define/c (map f) (foldr (∘ cons f) '()))
 
(map (* 2) '(1 2 3))                  ==>  '(2 4 6)
(map (map (* 2)) '((1 2) (3 4) (5)))  ==>  '((2 4) (6 8) (10))
```

Contract-based dynamical typing system
--------------------------------------

Being not so strict as in *Typed Racket* or *Haskell*, the contract typing system gives a flavor of rich type systems used in functional programming, including abstract, algebraic and polymorphic types.

```Scheme
(define-type Int-or-X
  integer?
  'X)
 
(is 5 Int-or-X)  ==> #t
(is 'X Int-or-X)  ==> #t
(is 'x Int-or-X)  ==> #f


(:: map ((a -> b) (list: a ..) -> (list: b ..))
  (define/c (map f)
    (/. (cons h t) --> (cons (f h) (map f t)))))
 
(map (* 2) '(1 21 4 3))  ==>  '(2 42 8 6)
```

Building abstract algebraic types with formal functions:

A formal constructor of a pair:

```Scheme
(define-formal kons)
 
(kons 1 (kons 2 3))  ==>  '(kons 1 (kons 2 3))
```
A constructor of the `klist`:

```Scheme
(define (klist . x)
  (foldr kons 'knull x))
 
(klist 'a 'b 'c)  ==>  '(kons a (kons b (kons c knull)))
```

A recursive type declaration:

```Scheme
(define-type klist?
  'knull
  (kons: Any klist?))
 
(is (klist 1 2 3) klist?)        ==>  #t
(is (list 1 2 3) klist?)         ==>  #f
(is (kons 1 (kons 2 3)) klist?)  ==>  #f
```

Some functions to operate with `klist`s:

```Scheme
(define/c (kfold f x0)
  (/. 'knull --> x0
      (kons h t) --> (f h (kfold f x0 t))))
 
(define/c (kfilter p) (kfold (fif (∘ p I1) kons I2) 'knull))
 
(define total (kfold + 0))
 
(kfold ($ 'f) 'x0 (klist 'x 'y 'z))   ==>  '(f x (f y (f z x0)))
(kfilter odd? (klist 1 2 3 4 5))      ==>  '(kons 1 (kons 3 (kons 5 knull)))
(total (klist 1 2 3))                 ==>  6
```

Monads
------

Monads give a very useful and elegant abstraction for sequential computations, allowing one to go deep into semantics, keeping syntax simple. Even though *Racket* doesn’t need monads to perform sequential computations, side effects or set comprehension, it is useful to have monads as a powerful tool for designing new semantic constructions.

An example of sequential computations in the `List` monad:

```Scheme
(do [x <- '(1 2 3)]
    [x <- (return (+ x 6))]
    (return x))               ==>  '(7 8 9)
```

A simple list comprehension:

```Scheme
(collect (sqr x) [x <- (in-range 6)] (odd? x))  ==>  '(1 9 25)
```

Monads allow to define generic functions and then to use them with different semantics. Here is a function returning a sequence of Pythagorean triples:

```Scheme
(define (find-triples r)
  (collect (list a b c)
    [a <- r]
    [b <- (in-range 1 a)]
    [c <-: (sqrt (- (sqr a) (sqr b)))]
    (integer? c)
    (> b c)))
```

One may use find-triples in eager `List` monad, or in lazy `Stream` monad, without changing a single line in the function definition.

```Scheme
(using List (find-triples (in-range 20)))   
==> 
'((5 4 3) (10 8 6) (13 12 5) (15 12 9) (17 15 8))
```

```Scheme
(define t
  (using Stream (find-triples (in-naturals))))
 
(stream-take t 5)
==>
'((5 4 3) (10 8 6) (13 12 5) (15 12 9) (17 15 8))

(stream-ref t 50)  ==>  '(100 80 60)
```

As in *Haskell*, it is possible to use pattern-matching in list-comprehensions

```Scheme
(define primitive-triples
  (using Stream
    (collect t
      [(and t (list a b _)) <- (find-triples (in-naturals))]
      [1 <-: (gcd a b)])))
 
(stream-take primitive-triples 5)
==>
'((5 4 3) (13 12 5) (17 15 8) (25 24 7) (29 21 20))
```

Consider a classical problem: *A farmer buys 100 animals for $100.00. The animals include at least one cow, one pig, and one chicken, but no other kind. If a cow costs $10.00, a pig costs $3.00, and a chicken costs $0.50, how many of each did he buy?*

Here is a declarative program solving this problem:

```Scheme
(define (solve-farmer-problem #:cow (c 10)
                              #:pig (p 3)
                              #:chicken (ch 1/2))
  (collect `((,cows cows) (,pigs pigs) (,chickens chickens))
    [cows     <-  (in-range 1 (/ 100 c))]
    [pigs     <-  (in-range 1 (/ 100 p))]
    [chickens <-: (- 100 cows pigs)]
    [= 100 (+ (* c cows) (* p pigs) (* ch chickens))]))
 
> (solve-farmer-problem)  ==>  '(((5 cows) (1 pigs) (94 chickens)))
```
If cow costs $8 there would be 6 possible solutions:

```Scheme
(solve-farmer-problem #:cow 8)
==>
'(((1 cows) (17 pigs) (82 chickens))
  ((2 cows) (14 pigs) (84 chickens))
  ((3 cows) (11 pigs) (86 chickens))
  ((4 cows) (8 pigs) (88 chickens))
  ((5 cows) (5 pigs) (90 chickens))
  ((6 cows) (2 pigs) (92 chickens)))
```

It is easy to create new monads. Here is an example of defining the parameterized monad `(Maybe a)`.

```Scheme
(define-formal Just)
 
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
                x       _ --> x)))
 
(using (Maybe Int)
  (bind (Just 2) >>= (lift sqr) >>= (lift (* 2))))  
==>  
'(Just 8)
  
(using (Maybe Int)
  (bind 'Nothing >>= (lift sqr) >>= (lift (* 2))))  
==> 
'Nothing
```

This monad could be used to perform guarded computations:

```Scheme
(define (safe-sqrt x)
  (bind (return x) >>=
        (guardf positive?) >>=
        (lift sqrt)))
 
(using (Maybe Real)
  (map safe-sqrt  '(-1 4 3 9 -4 -9))) 
==>
'(Nothing (Just 2) (Just 1.7320508075688772) (Just 3) Nothing Nothing)
```

Formica provides monadic functions and operators, such as `lift/m`, `compose/m`, `fold/m`, `map/m`, etc.

```Scheme
(using-monad List)

(lift/m or '(#t #f) '(#t #f))  ==>  '(#t #t #t #f)

(fold/m (λ (x y) `((+ ,x ,y) (- ,x ,y))) 0 '(1 2 3))
==>
'((+ 3 (+ 2 (+ 1 0)))
  (- 3 (+ 2 (+ 1 0)))
  (+ 3 (- 2 (+ 1 0)))
  (- 3 (- 2 (+ 1 0)))
  (+ 3 (+ 2 (- 1 0)))
  (- 3 (+ 2 (- 1 0)))
  (+ 3 (- 2 (- 1 0)))
  (- 3 (- 2 (- 1 0))))

(define powerset (filter/m (const '(#t #f))))
 
(powerset '(1 2 3)) 
==>
'((1 2 3) (1 2) (1 3) (1) (2 3) (2) (3) ())
```

Monads could be created through higher order abstractions:

```Scheme
(define-monad Set
  (Monoid
   #:return set
   #:mplus set-union))
 
(using Set
  (lift/m + '(1 2 3) '(2 3 4)))  ==>  (set 3 4 5 6 7)


(define-monad String
 (Monoid
  #:return string
  #:mplus (flipped string-append)))
 
(using String
 (collect (char-upcase x)
   [x <- "abc12xy"]
   (char-alphabetic? x)
   [x <- (format "~a(~a)" x (char->integer x))]))
==>
"A(97)B(98)C(99)X(120)Y(121)"
```

Miscellaneous functional tools
------------------------------

Formica is informally called to be "functionally-oriented", meaning that it provides a lot of tools to operate with functions.

This defines binary and unary formal functions:

```Scheme
(define-formal (b 2) (u 1))
```

Here is a generalized composition of them:

```Scheme
((∘ b u) 1 2)  ==>  '(b (u 1) 2)
((∘ u b) 1 2)  ==>  '(u (b 1 2))
```

The generalized composition is associative and has identity function as a neutral element:

```Scheme
(equal? ((∘ (∘ b u) b) 'a 'b 'c)
        ((∘ b (∘ u b)) 'a 'b 'c)
        (b (u (b 'a 'b)) 'c))      
==>  #t

(equal? ((∘ id b) 1 2)
        ((∘ b id) 1 2)
        (b 1 2))         
==>  #t
```
Some purely functional definitions

```Scheme
(define max (foldr (fif < I2 I1) -inf.0))
 
(max '(2 3 1 2 3 2 5 3 2))  ==>  5
```

```Scheme
(define/c (all-elements p) (foldr (∘ and p) #f))
 
(all-elements odd? '(2 4 5 3 7 8))  ==>  #f
(all-elements even? '(2 4 6 0 12 8))  ==> #f
```

```Scheme
(define all-arguments (/@ and))
(define complex<? (andf (all-arguments complex?)
                        (/@ < magnitude)))
 
(complex<? 0-1i 2 4+1i) ==> #t
(complex<? 2 1-1i)      ==> #f
(complex<? 2 'x 1-1i)   ==> #f
```



What do I need to build/compile/interpret Formica programs?
-----------------------------------------------------------

The Formica can be interpreted by the `racket` program or in the DrRacket IDE. It could be loaded as a package using `(require formica)` or as a language using `#lang formica` header.

To build and install Formica in the DrRacket environment follow one of possible ways:
 
1. For users

   Download the `formica.plt` package from this repository and install it using File|Install .plt-file in the DrRacket menu, or using 'raco setup'.

2. For possible contributors

   Clone this repository to any working directory using

   ```
   git clone https://github.com/samsergey/formica.git
   ``` 

   and then install package using

   ```
   make install
   ``` 

   or manually by running 

   ```
   raco link formica
   raco setup formica
   [sudo] raco setup -U
   ```

   from the parent folder of the working one.

---------------------------------------------------------------

Everyone is welcome to comment, discuss and make pull requests!

Sergey B. Samoylenko (samsergey `at` yandex `dot` ru)
