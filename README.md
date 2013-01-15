Formica
=======

A "functionally-oriented" Racket dialect.

Introduction
------------

The Formica dialect was created for educational purposes while teaching 
the "Functional and logical programming" undergraduate course in the 
Kamchatka State Technical University (Russia).

It is based on the [Racket programming language](http://planet.racket-lang.org/), famous by it's educational and practical use.


Why Racket?
-----------

  a) The transparency of *Scheme* (Racket) programs: **use only words you need to express yourself**.
  
  b) The symbols are first-class objects: **you see what happens in the program**.

  c) The ease of creating domain specific dialects and languages: **say exactly what you mean by your programs**.
  
  d) The brilliant IDE DrRacket: **everything is in one thing and nothing is excess**.

  e) Racket has active community, reach libraries and provides a lot of real-life instruments for GUI development, web tools etc. **Racket is a programming language**

Why Formica?
------------

The main goal of designing Formica is to have a functional programming language as flexible as *Racket* or *Wolfram Mathematica*, and almost as syntactically clean as Mark Tarver’s *Qi* or *Haskell*. Being a dialect of *Racket* it should complement the parent language and make it possible to use any of *Racket*’s native libraries.

Even though it is mainly educational language, some of it's features (such as **formal functions**, **abstract rewriting systems** etc.) could be used in various practical applications.

What is inside?
---------------

For a brief tour and complete documentation see the 'formica.pdf' file if the 'doc' directory.

What do I need to build/compile/interpret Formica programs?
-----------------------------------------------------------

The Formica can be interpreted by the `racket` program or in the DrRacket IDE. It could be loaded as a package using `(require formica)` or as a language using `#lang formica` header.

To build and install Formica in the DrRacket environment follow one of possible ways:
 
 1) (For users) 

 Download the 'formica.plt' package from this repository and install it using File|Install .plt-file in the DrRacket menu, or using 'raco setup'.

 2) (For possible contributors) 

 Clone this repository to any working directory using 
 ```
 git clone https://github.com/samsergey/formica.git
 ``` 
 and then link this directory using `raco link` program. Finally run `raco setup` to build documentation and include formica modules in Racket module resolving system.

---------------------------------------------------------------

Every one is welcome to comment and discuss!

Sergey B. Samoylenko (samsergey `at` yandex `dot` ru)
