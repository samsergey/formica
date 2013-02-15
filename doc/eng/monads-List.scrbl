#lang scribble/doc

@(require (for-label formica))

@(require
  scribble/manual
  scribble/eval)

@(define formica-eval
   (let ([sandbox (make-base-eval)])
     (sandbox '(require formica))
     sandbox))

@title{Monads, defined in Formica}

@declare-exporting[formica]

@section{The Identity monad}

@defthing[Id monad?] 
The identity monad.

Definition:
@codeblock{return = id
           bind _m _f = (_f _m)}

Examples:
@defs+int[#:eval formica-eval
 ((using-monad Id)
 (define-formal f))
 (return 'x)
 (bind 'x >>= f)]

In the @racket[Id] monad @racket[do] form works like @racket[match-let*] form with ability to produce side effects within calculations:
@interaction[#:eval formica-eval
 (do [x <- 5]
     [y <- 8]
     (displayln x)
     [x <- (+ x y)]
     (list x y))]

@interaction[#:eval formica-eval
 (do [(cons x y) <- '(1 2 3)]
     [(z t) <<- (reverse y)]
     (return (list x y z t)))]

@section{Ambiguous computations.}

@defproc[(Monoid [#:return ret (Any ... → listable?)]
                   [#:mplus seq-append (listable? listable? → listable?)]
                   [#:map seq-append-map ((Any → listable?) listable? → listable?) mplus-map]) monad-plus?]
Returns a monad which performs computations which may return zero or more then one possible result as a sequence (list, stream, set etc.). This is an abstraction of monads operating with monoids. Monads @racket[List], @racket[Stream] and @racket[Amb] are defined through the @racket[Monoid] function.

Definition:
@codeblock{return = _ret
           bind _s _f = (_seq-append-map _f _s)
           mplus = _seq-append
           mzero = (return)
           type = listable?
           failure = (const mzero)}

The sequence of arguments is constructed by the @racket[_ret] function. The bound function @racket[_f] is applied to all possible values in the input sequence @racket[_s] and the resulting sequences are concatenated by the @racket[_seq-append] to produce a sequence of all possible results. The details of binding implementation are specified by the mapping function @racket[_seq-append-map].

The @racket[Monoid] function creates monads, operating with @emph{monoids}: sets having a single identity element @racket[(_ret)], and an associative binary operator over the set of objects @racket[_seq-append].

@bold{Examples:}

Using @racket[Monoid] it is easy to define monads working with different types of sequences: sets, vectors, strings etc.
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

@defproc[(listable? (v Any)) Bool]
Returns @racket[#t] if @racket[v] is a sequence, except for @tech{formal applications} or integers.

Examples:
@interaction[#:eval formica-eval
  (listable? '(1 2 3))
  (listable? 4)
  (listable? (stream 1 2 (/ 0)))
  (listable? '(g x y))
  (define-formal g)
  (listable? (g 'x 'y))]

@defproc[(mplus-map [f (Any → listable?)] [s listable?]) listable?]
Generalized mapping function for the @tech{currently used monad}. Formally equal to 
@codeblock{(mplus-map f s) = (foldl (∘ mplus f) mzero s)}

@defproc[(zip [s listable?] ...) sequence?]
Returns a sequence where each element is a list with as many values as the number of supplied sequences; the values, in order, are the values of each sequence. Used to process sequences in parallel, as in @racket[for/list] iterator.

Example of using @racket[zip]:
@interaction[#:eval formica-eval
  (using List
    (do [(list x y) <- (zip '(a b c) 
                            '(1 2 3 4))]
      (return (f x y))))]
The same with @racket[for/list] iterator.
@interaction[#:eval formica-eval
 (for/list ([x '(a b c)]
            [y '(1 2 3 4)])
   (f x y))]

@subsection{The List monad}

@defthing[List monad-plus?] 
The @racket[List] monad is used for list comprehension and in order to perform calculations with functions, returning more then one value. The main difference between the @racket[List] and monad @tt{[]} in @emph{Haskell}, is the ability of @racket[List] to operate with any sequences as @racket[for] iterators do.

Definition:
@codeblock{List = (Monoid #:return list
                          #:mplus append
                          #:map append-map)}

@bold{Examples}

@def+int[#:eval formica-eval
 (using-monad List)]

Examples of list comprehension
@interaction[#:eval formica-eval
 (collect (sqr x) [x <- '(1 2 5 13 4 24)] (odd? x))]

@interaction[#:eval formica-eval
 (collect (cons x y) 
   [x <- '(1 3 4 8 2 4)]
   [y <- '(3 1 6 4 3)]
   (< x y)
   (= (modulo x y) 2))]

@interaction[#:eval formica-eval
 (collect (cons x y) 
   [(list x y) <- '((1 2) (2 4) (3 6) (5 1))]
   (odd? x)
   (< x y))]

Forms @racket[do] and @racket[collect] in the @racket[List] monad work like @racket[for*/list] form:
@interaction[#:eval formica-eval
 (do [x <- '(1 2)] 
     [y <- '(a b c)] 
     (return (cons x y)))
 (for*/list ([x '(1 2)] 
             [y '(a b c)]) 
   (cons x y))]

@interaction[#:eval formica-eval
 (collect (cons x y) 
   [x <- '(1 2 3)] 
   (odd? x) 
   [y <- '(a b c)])
 (for*/list ([x '(1 2 3)] 
             #:when (odd? x)
             [y '(a b c)]) 
   (cons x y))]

The use of monad @racket[List] goes beyond the simple list generation. The main purpose of monadic computations is to provide computation with functions which may return more then one value (or fail to produce any). The examples of various applications of this monad could be found in the @filepath{nondeterministic.rkt} file in the @filepath{examples/} folder.

@subsection{The Stream monad}

@defthing[Stream monad-plus?] 
Like @racket[List] monad, but provides lazy list processing. This monad is equivalent to monad @tt{[]} in @emph{Haskell} and could be used for operating with potentially infinite sequences.

Definition:
@codeblock{Stream = (Monoid #:return list
                            #:mplus stream-concatenate
                            #:map stream-concat-map)}

@defproc[(stream-concatenate (s listable?) ...) stream?]
Returns a result of @racket[_s ...] lazy concatenation in a form of a stream.

Examples:
@interaction[#:eval formica-eval
  (stream->list 
   (stream-concatenate '(1 2 3) '(a b c)))
  (stream->list 
   (stream-concatenate (in-range 4) (stream 'a 'b 'c)))
  (stream-ref 
   (stream-concatenate (stream 1 (/ 0)) (in-naturals)) 
   0)
  (stream-ref 
   (stream-concatenate (stream 1 (/ 0)) (in-naturals)) 
   1)]

@defproc[(stream-concat-map (f (Any → stream?)) (s listable?)) stream?]
Applies @racket[_f] to elements of @racket[_s] and lazily returns the concatenation of results.

Examples:
@interaction[#:eval formica-eval
  (stream->list 
   (stream-concat-map (λ (x) (stream x (- x))) '(1 2 3)))
  (stream->list 
   (stream-concat-map (λ (x) (stream x (- x))) (in-range 4)))
  (stream-ref 
   (stream-concat-map (λ (x) (stream x (/ x))) '(1 0 3)) 
   0)
  (stream-ref 
   (stream-concat-map (λ (x) (stream x (/ x))) '(1 0 3)) 
   1)
  (stream-ref 
   (stream-concat-map (λ (x) (stream x (/ x))) '(1 0 3)) 
   2)
  (stream-ref 
   (stream-concat-map (λ (x) (stream x (/ x))) '(1 0 3)) 
   3)]

@defproc[(stream-take (s stream?) (n Nat)) list?]
Returns list of @racket[_n] first elements of stream (or sequence) @racket[s].

Examples:
@interaction[#:eval formica-eval
  (stream-take (stream 'a 'b 'c) 2) 
  (stream-take (stream 'a 'b (/ 0)) 2)
  (stream-take (in-naturals) 3)]

@bold{Examples}

@def+int[#:eval formica-eval
 (using-monad Stream)]

Two classical examples with infinite sequences.

The infinite sequence of Pythagorean triples:
@def+int[#:eval formica-eval
 (define triples 
  (collect (list a b c) 
    [a <- (in-naturals)]
    [b <- (in-range (ceiling (/ a 2)) a)]
    [c <-: (sqrt (- (sqr a) (sqr b)))]
    (integer? c)
    (> b c)))
(stream-take triples 3)
(stream-ref triples 100)]

The first triangle with area exceeding 100: 
@interaction[#:eval formica-eval
  (stream-first 
   (collect t 
     [(and t (list _ b c)) <- triples] 
     (> (* 1/2 b c) 100)))]


The infinite sequence of primes:
@def+int[#:eval formica-eval
(define (primes r)
  (do [(scons x xs) <-: r]
      [p <-: (collect y 
               [y <- xs] 
               (not (zero? (modulo y x))))]
      (stream-cons x (primes p))))

(stream-take (primes (in-naturals 2)) 10)
(stream-ref (primes (in-naturals 2)) 100)]

Using monad @racket[Stream] all monadic functions work lazily however they still operate on eager lists:
@interaction[#:eval formica-eval
 (stream-first ((compose/m (lift /) (lift (curry * 2))) 1/2 0))
 (stream-first (lift/m / (return 1) (return 1 0)))
 (stream-ref (lift/m / (return 1) (return 1 0)) 1)
 (stream-ref (map/m (λ (x) (stream x (/ x))) '(1 0 3)) 0)
 (stream-ref (map/m (λ (x) (stream x (/ x))) '(1 0 3)) 1)
 (stream-ref (map/m (λ (x) (stream x (/ x))) '(1 0 3)) 2)]

@subsection{The Amb monad}

@defthing[Amb monad-plus?] 
Like @racket[Stream] monad, but tries to return a list of unique elements.

Definition:
@codeblock{Amb = (Monoid #:return amb
                         #:mplus amb-union
                         #:map amb-union-map)}

@defproc[(amb (v Any) ...) stream?]
Returns a stream of arguments @racket[_v] removing duplicates (in terms of @racket[equal?]).

Examples:
@interaction[#:eval formica-eval
 (stream->list (amb 1 3 2 2 3 2 1 2 4 3))
 (stream-take (amb 1 2 (/ 0)) 2)]

@defproc[(amb-union (s1 listable?) (s2 listable?)) stream?]
Returns a stream of elements from @racket[_s1] and @racket[_s2], removing duplicates.

Examples:
@interaction[#:eval formica-eval
  (stream->list 
   (amb-union '(1 2 3) '(2 3 4)))
  (stream->list 
   (amb-union (in-range 4) (amb 'a 'b 'c)))
  (stream-ref 
   (amb-union (amb 1 (/ 0)) (in-naturals)) 
   0)
  (stream-ref 
   (amb-union (amb 1 (/ 0)) (in-naturals)) 
   1)]

@defproc[(amb-union-map (f (Any → stream?)) (s listable?)) stream?]
Applies @racket[_f] to elements of @racket[_s] and lazily returns the union of results.

Examples:
@interaction[#:eval formica-eval
  (stream->list 
   (amb-union-map (lift sqr) '(-3 -2 -1 0 -1 2 3)))
  (stream->list 
   (amb-union-map (λ (x) (amb x (- x))) (in-range 4)))
  (stream-ref 
   (amb-union-map (λ (x) (amb x (/ x))) '(1 0 3)) 
   0)
  (stream-ref 
   (amb-union-map (λ (x) (amb x (/ x))) '(1 0 3)) 
   1)
  (stream-ref 
   (amb-union-map (λ (x) (amb x (/ x))) '(1 0 3)) 
   2)]

@bold{Examples}

@def+int[#:eval formica-eval
 (using-monad Amb)]

Proving logical statements by brute force
@interaction[#:eval formica-eval
 (stream->list 
  (collect (eq? (==> A B) (or B (not A))) 
    [(A B) <<- (amb #t #f)]))
 (stream->list 
  (collect (==> (==> (==> A B) C) (==> A (==> B C)))
    [(A B C) <<- (amb #t #f)]))
 (stream->list 
  (collect (eq? (==> (==> A B) C) (==> A (==> B C)))
    [(A B C) <<- (amb #t #f)]))
 (stream->list 
  (collect (list A B C)
    [(A B C) <<- (amb #t #f)]
    (not (eq? (==> (==> A B) C) (==> A (==> B C))))))]