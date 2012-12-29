Formica
=======

A "functionally oriented" Racket dialect.

Introduction
------------

The Formica dialect was created for educational purposes while teaching 
the "Functional and logical programming" undergraduate course in the 
Kamchatkan State Technical University (Russia).

It is based on the [Racket programming language](http://planet.racket-lang.org/), famous by it's educational and practical use.

The main goal of designing the Formica language is to have a functional programming language as flexible as *Racket* and *Wolfram Mathematica*, and almost as syntactically clean as *Qi* or *Haskell*.

Even though it is mainly educational language, some of it's feaures (such as **formal functions**, **abstract rewriting systems**, **generalized composition** etc.) could be used in various practical applications.

Why Racket?
-----------

  a) The transparency of *Scheme* (Racket) programs: **use only words you need to express yourself**.
  
  b) The symbols are first-class objects: **you see what happens in the program**.

  c) The ease of creating domain specific dialects and languages: **say exactly what you mean by your programs**.
  
  d) The brilliant IDE DrRacket: **everything is in one thing and nothing is excess**.

Why Formica?
------------

Here are the main features of the Formica dialect.

  **a) Formal functions** (almost as in *Wolfram Mathematica*):

This declares `f` to be a formal function
```Scheme
(define-formal f)
```

Now we can see how some high-order functions work:
```Scheme
(map f '(a b c)) ==> '((f a) (f b) (f c))
(foldr f 'x0 '(a b c)) ==> '(f b (f c (f a x0)))
```
In the following example the `+` function is made formal:
```Scheme
(foldr (hold +) 0 '(1 2 3)) ==> '(+ 3 (+ 2 (+ 1 0)))
```

  **b) Rewriting** (almost as in *Wolfram Mathematica*):
  
This rewrites 1 to 2, 3 to 4, and 4 to 1.
```Scheme
(define r 
  (/. 1 --> 2
      3 --> 4
      4 --> 1))

(map r '(1 2 3 4)) ==> '(2 2 4 1)
```

This rewrites 1 to 2, 3 to 4, and 4 to 1 repeatedly. The first rule is terminal (the rewriting process stops here).
```Scheme
(define r 
  (//. 1 -->. 2
       3 --> 4
       4 --> 1))

(map r '(1 2 3 4)) ==> '(2 2 2 2)
```

This is a rewriting-based difinition of the `map` function:
```Scheme
  (define map
    (/. f (cons h t) --> (cons (f h) (map f t))
       _ '() --> '()))
```
  
  **c)** Simplified syntax for **partial application** (as in *Qi*):
  
```Scheme
  (map (* 2) '(1 2 3)) ==> '(2 4 6)
  (map (map (* 2)) '((1 2) (3 4) (5))) ==> '((2 4) (6 8) (10))
```

  **d) Tacit definitions** (almost as in *Haskell*):

```Scheme
  (define/c (map f) (fold (∘ cons f) '()))
  (define/c (fold f x0)
    (/. (cons h t) --> (f h (fold f x0 t))
        '() --> x0))
```

  **e)** Contract-based **dynamical typing system**. Not strict, but instructive:

 ```Scheme
 (define-type natural?
  (and/c integer? positive?))

 (is 5 natural?) ==> #t

 (:: add1 (natural? -> natural?)
   (define (add1 n) (+ 1 n)))
 ``` 
  
 Building abstract algebraic types with formal functions:
```Scheme
; a formal constructor of a pair
(define-formal kons)

; a constructor of the klist
(define (klist . x)
  (foldr kons 'knull x))

; a recursive type declaration
(define-type klist?
  'knull
  (kons: Any klist?))

; some functions to operate with klists
(define/c (kfold f x0)
  (/. 'knull --> x0
      (kons h t) --> (f h (kfold f x0 t))))

(define total (kfold + 0))

(kfold (hold f) 'x0 (klist 'x 'y 'z)) ==> '((f z (f y (f x x0))))
(total (klist 1 2 3)) ==> 6
```

  **f)** A *Haskell*-like syntax for dealing with **monads**:

```Scheme
(do [x <- '(1 2 3)]
    [x <- (return (+ x 6))]
    (return x)))                     ==>  '(7 8 9)

(collect (sqr x) [x <- '(1 2 3 4)])  ==> '(1 4 9 16)
(collect (sqr x) [x <- 6] (odd? x))  ==> '(1 9 25)
```

Switch between monads:

```Scheme
(collect (+ x y)
    [(x y) <- '(1 2 3 4)]
    (odd? x))               ==> '(2 3 4 5 4 5 6 7)

(using Set
  (collect (+ x y)
    [(x y) <- '(1 2 3 4)]
    (odd? x)))              ==>  (set 2 3 4 5 6 7)


(using Amb
  (collect (even? (+ x y))
    [(x y) <- '(1 2 3 4)]
    (odd? x)))              ==> '(2 . #<promise>)
```

Define new monads:

```Scheme
(define-monad-plus Or
  #:return (fif boolean? id in-value)
  #:bind (/. #f _ --> #f
             #t f --> (f #t)
              m f --> (for/or ([x m]) (f x)))
  #:mzero #f
  #:mplus (λ (a b) (or a b)))

(using Or
  (collect (even? (+ x y))
    [(x y) <- '(1 2 3 4)]
    (odd? x)))              ==> #t
```

  **g)** A handfull of functional tools:

A **generalized function composition**:

This defines binary and unary formal functions:
```Scheme
(define-formal (b 2) (u 1))
```

Here is a composition of them:
```Scheme
((∘ b u) 1 2) ==> `(b (u 1) 2)
((∘ u b) 1 2) ==> `(u (b 1 2))
```
The generalized composition is associative and has identity functiona as a neutral element:
```Scheme
((∘ (∘ b u) b) 1 2 3) ==> `(b (u (b 1 2)) 3)
((∘ b (∘ u b)) 1 2 3) ==> `(b (u (b 1 2)) 3)
((∘ id b) 1 2)        ==> `(b 1 2)
((∘ b id) 1 2)        ==> `(b 1 2)
```

Here are purely functional definitions of `max` and `filter` functions
```Scheme
(define/c (max) (foldr (fif < I2 I1) -inf.0)) 
(define/c (filter p) (foldr (fif (∘ p I1) cons I2) '())) 
```

What do I need to build/compile/interpret Formica programs?
-----------------------------------------------------------

The Formica is a dialect of the [Racket](http://planet.racket-lang.org/) language and could be interpreted by the `racket` program or in the DrRacket IDE. It could be loaded as a package using `(require formica)` or as a language using `#lang formica` header.

To build Formica in the DrRacket environment clone this repository to any working directory using `git` and then link this directory using `raco link` program. Finally run `raco setup` to build documentation and include formica modules in Racket module resolving system.


Why package is not complete?
----------------------------

The language modules are already written, fully documented in Russian, and were used by students during two terms. 

However before committing the Formica on the GitHub I would like to revise each module, make more or less complete tests and have even brief English documentation for all of them. 

So, starting from the New Year all modules will appear here one by one. And before the next term starts in February, the working Formica package should be complete and ready for use, branching and discussions.

Every one is welcome to comment and discuss!

Sergey B. Samoylenko (samsergey `at` yandex `dot` ru)
