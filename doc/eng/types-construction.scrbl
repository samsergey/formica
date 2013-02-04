#lang scribble/doc

@(require (for-label formica))

@(require
  scribble/manual
  scribble/eval)

@(define formica-eval
   (let ([sandbox (make-base-eval)])
     (sandbox '(require formica))
     sandbox))

@title[#:tag "type:definition"]{Defining new types}

@defform*[[(define-type name)
           (define-type (name c ...))
           (define-type name c ...)
           (define-type (name x ...) c ...)] #:contracts ([c Type] [x Type])]
Defines named abstract, algebraic and parameterized types.
@itemize{@item{@racket[(define-type name)] --- defines an abstract container type as a @tech{formal function}.}
         @item{@racket[(define-type (name c ...))] --- defines an algebraic type, being a type product of @racket[_c ...].}
         @item{@racket[(define-type name c ...)] --- defines an algebraic type, being a type sum of @racket[_c ...].}
         @item{@racket[(define-type (name x ...) c ...)] --- defines a parameterized type.}}


@section[#:tag "types:type combinators"]{Contract combinators}
@deftech{Contract combinators} allow to construct new types out of existing @tech{primitive types} or @tech{container types}.

@defproc[(Any [v Any]) Type]
The contract for any value.

@deftogether[
[@defproc[(∪ [c Type] ...) Type]
 @defproc[(∩ [c Type] ...) Type]
 @defproc[(\\ [c Type] ...+) Type]]]
Union, intersection and complement of contracts.

@section[#:tag "types:ADT"]{Container types}

Algebraic data types could be created using @deftech{container types}: pairs, lists, @tech{formal functions} or structures.

@defproc[(cons: [c1 Type] [c2 Type]) Type]
The contract for a pair of values, which belong to types @racket[_c1] and @racket[_c2].

@interaction[#:eval formica-eval
  (is (cons 1 2) (cons: 1 2))
  (is (cons 1 2) (cons: Num Num))
  (is (cons 1 'x) (cons: Num Num))
  (is (cons 1 (cons 2 'x)) (cons: Num (cons: Num Sym)))]

@defform[(list: c ...)]
The contract for lists of elements having types specified by contracts @(racket _c ...). The contract specification could be given as a domain of a funtion.

Examples:

@interaction[#:eval formica-eval
 (is '(1 2) (list: 1 2))
 (is '(1 2) (list: Num Num))
 (is '(1 2) (list: Num Sym))
 (is '(1 2 -3) (list: positive? positive? negative?))]

@interaction[#:eval formica-eval
 (is '(1 2) (list: Num ..))
 (is '(1 1 1 1) (list: 1 ..))
 (is '(1 2 x 4 5) (list: Num ..))
 (is '(1 2 30 1) (list: positive? ..))]
                                       
@interaction[#:eval formica-eval
 (is '(1 2 3) (list: 1 Num ..))
 (is '(1 2 2 2) (list: 1 2 ..))
 (is '(1 2 x 4 5) (list: 1 2 Num ..))]

@interaction[#:eval formica-eval
 (is '(1) (list: 1 (? 2 3)))
 (is '(1 2) (list: 1 (? 2 3)))
 (is '(1 2 3) (list: 1 (? 2 3)))
 (is '(1 2 3 4) (list: 1 (? 2 3)))]


@defform/none[(f: dom ...)]
contract for the @tech{formal application} of function @racket[_f] having domain specified by @(racket _dom). Similar to @racket[list:] combinator.

All contracts defined in the @racket[racket/contract] module also could be used. 

@bold{Example 1.}

Special types could be defined as usual predicate functions:
 @def+int[#:eval formica-eval
  (define-type Nat 
    (andf integer? positive?))
  (is 9 Nat)
  (is -4 Nat)
  (is "abc" Nat)]

To deal with unit types use contract combinators:
 @def+int[#:eval formica-eval
  (define-type Nat*
    (\\ Nat 4))
  (is 3 Nat*)
  (is 4 Nat*)
  (is 5 Nat*)]

 
@bold{Example 2.}

Enumerable type "friend":

@def+int[#:eval formica-eval
  (define-type Friend
    "Andrew" "Mary" "Park Hae Dong" "James")
  (is "Mary" Friend)
  (is "Gregory" Friend)
  (is 845 Friend)]

@bold{Example 3.}

A simple container type:
@def+int[#:eval formica-eval
 (define-type A)
 (A 6)
 (is (A 'x 'y) A?)
 (is (A 'x 'y) (A: Sym Sym))
 ((/. (A x y) --> (+ x y)) (A 2 4))]

A simple container type with specified type of argumets:
@def+int[#:eval formica-eval
 (define-type (A Int))
 (A 6)
 (A 'x)
 (is (A 6) A?)
 (is '(A x) A?)]

Inductive type "List":

@def+int[#:eval formica-eval
  (define-type List 
    null?
    (cons: Any List))
  (is '(a b c) List)
  (is (cons 'a (cons 'b (cons 'c '()))) List)
  (is (cons 'a (cons 'b 'c)) List)
  (is 42 List)]

@bold{Example 4.}

Inductive parametrized type: "List of A":
@def+int[#:eval formica-eval
  (define-type (Listof A)
    null?
    (cons: A (Listof A)))
  (is '(1 2 3) (Listof Int))
  (is '(1 2 x) (Listof Int))
  (is '(a b c) (Listof Sym))]

@bold{Example 5.}

This defines an abstract data type equivalent to a @racket[cons]-pair:

@def+int[#:eval formica-eval
  (define-type (kons Any Any))
  (kons 1 2)
  (kons (kons 1 2) (kons 3 4))]

@interaction[#:eval formica-eval
  (is (kons 1 2) kons?)
  (is (cons 1 2) kons?)
  (is 42 kons?)]

Here is a type definition and the constructor of lists, based on @racket[kons]-pairs. We use the @racket['knull] symbol as an empty list: @racket[klist]s:
@def+int[#:eval formica-eval
  (define-type klist? 
    'knull
    (kons: Any klist?))   
  (is (kons 1 (kons 2 'knull)) klist?)
  (is (kons 1 (kons 2 3)) klist?)
  (is (list 1 2 3) klist?)]

@def+int[#:eval formica-eval
  (define (klist . x)
    (foldr kons 'knull x))
  (klist 1 2 3 4)
  (is (klist 1 2 3) klist?)
  (is (klist) klist?)]

Finally let's define folding of @racket[klist]s and it's ancestors:

@def+int[#:eval formica-eval
  (define/c (kfold f x0)
    (/. 'knull --> x0
        (kons h t) --> (f h (kfold f x0 t))
        x --> (error "kfold: The argument must be a klist. Given" x)))]

@defs+int[#:eval formica-eval
  ((define/c (kmap f) (kfold (∘ kons f) 'knull))
  (define total (kfold + 0)))
  (kmap sqr (klist 1 2 3 4 5))
  (total (klist 1 2 3 4 5))
  (kmap sqr (list 1 2 3 4 5))]

@bold{Example 6.}

Let's define a parametrized abstract algebraic type to represent a binary tree having leaves of given type:
@racketgrammar[#:literals (Empty) (Tree A) Empty (Leaf A) (Node (Tree A) (Tree A))]

@defs+int[#:eval formica-eval
  ((define-type (Leaf Any))
  (define-type (Node Any Any))
  (define-type (Tree A) 
    'Empty
    (Leaf: A)
    (Node: (Tree A) (Tree A))))]

Now we are able to create binary trees:
@defs+int[#:eval formica-eval
  [(define A (Node (Node (Leaf 5) 
                        'Empty) 
                  (Node (Node 'Empty 
                              (Leaf 6)) 
                        (Leaf 2))))
  (define B (Node (Leaf 'a) 
                         (Node (Leaf 'b) 
                               (Node (Leaf 'c)
                                     'Empty))))]
  (is A (Tree Int))
  (is A (Tree Sym))
  (is B (Tree Sym))]

Let's define folding function for binary trees:
@def+int[#:eval formica-eval
  (define/c (tfold f x0 g)
    (/. 'Empty --> x0
        (Leaf x) --> (g x)
        (Node l r) --> (f (tfold f x0 g l)
                          (tfold f x0 g r))
        x --> (error "The argument must be a Tree. Given" x)))]

Using folding it is possible to define a number of different functions:

Mapping:
@def+int[#:eval formica-eval
   (define/c (tmap f) (tfold Node 'Empty (∘ Leaf f)))
   (tmap sqr A)]

List of leaves
@def+int[#:eval formica-eval
   (define leaves (tfold append '() list))
   (leaves A)
   (leaves B)
   (leaves '(1 2 3))]

Sum of elements (for numeric entries):
@def+int[#:eval formica-eval
   (define (total t)
     (cond
       [(is t (Tree Num)) (tfold + 0 id t)]
       [else (error "total: The argument must be a (Tree Num). Given" t)]))
   (total A)
   (total B)]