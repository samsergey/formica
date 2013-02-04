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

For a brief tour and complete documentation see the `formica.pdf` file in the 'doc' directory.

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
