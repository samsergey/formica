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

@defform*[[(define-type name c ...)
  (define-type (name x ...) c ...)] #:contracts ([c contract?] [x contract?])]
Defines named contract which returns @racket[#t], if the argument satisfies any contract within @racket[_c ...]. If parameters @racket[_x ...] are given, defines the contract for parametrized type.

Types defined by @racket[define-type] represent @deftech{algebraic types}, where the sequence @nonbreaking{@racket[_c ...]} represents the @emph{type sum}, @tech{container types}  correspond to @emph{type products} and @tech{primitive types} correspond to @emph{unit types}.

@section[#:tag "types:type combinators"]{Contract combinators}
@deftech{Contract combinators} allow to construct new types out of existing @tech{primitive types} or @tech{container types}.

@defproc[(Any [v Any]) contract?]
The contract for any value.

@deftogether[
[@defproc[(or/c [c contract?] ...) contract?]
 @defproc[(and/c [c contract?] ...) contract?]
 @defproc[(not/c [c contract?]) contract?]]]
Union, intersection and negation of contracts.

@section[#:tag "types:ADT"]{Container types}

Algebraic data types could be created using @deftech{container types}: pairs, lists, @tech{formal functions} or structures.

@defproc[(cons: [c1 contract?] [c2 contract?]) contract?]
The contract for a pair of values, which belong to types @racket[_c1] и @racket[_c2].

@interaction[#:eval formica-eval
  (is (cons 1 2) (cons: 1 2))
  (is (cons 1 2) (cons: Num Num))
  (is (cons 1 'x) (cons: Num Num))
  (is (cons 1 (cons 2 'x)) (cons: Num (cons: Num Sym)))]

@defform*[
[(list: c ...) 
 (list: c ..)] #:contracts [(c contract?)]]
The contract for lists.

@itemize{ @item{@racket[(list: c ...)] contract for a list with fixed number of elements.
                 The number of elements must be equal to a number of contracts @racket[_c ...] and all elements
                 must satisfy corresponding contracts.
                 @interaction[#:eval formica-eval
                   (is '(1 2) (list: 1 2))
                   (is '(1 2) (list: Num Num))
                   (is '(1 2) (list: Num Sym))
                   (is '(1 2 -3) (list: positive? positive? negative?))]}
          @item{@racket[(list: c ..)] contract for list of elements, satisfying contract @racket[c].
                 @interaction[#:eval formica-eval
                   (is '(1 2) (list: Num ..))
                   (is '(1 1 1 1) (list: 1 ..))
                   (is '(1 2 x 4 5) (list: Num ..))
                   (is '(1 2 30 1) (list: positive? ..))]}}

@defform/none[(f: c ...) #:contracts [(c contract?)]]
contract for the @tech{formal application} of function @racket[_f]. Similar to @racket[list:] combinator.

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
    (and/c (not/c 4) integer? positive?))
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
  (define-formal kons)
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
As a type constructors @racket[Leaf] and @racket[Node] we will use formal functions, the unit type @racket[Empty] will be represented by a symbol @racket['Empty].

@def+int[#:eval formica-eval
  (define-formal Leaf Node)]

@def+int[#:eval formica-eval
  (define-type (Tree A) 
    'Empty
    (Leaf: A)
    (Node: (Tree A) (Tree A)))]

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

Sum of elements (for numebric entries):
@def+int[#:eval formica-eval
   (define (total t)
     (cond
       [(is t (Tree Num)) (tfold + 0 id t)]
       [else (error "total: The argument must be a (Tree number?). Given" t)]))
   (total A)
   (total B)]