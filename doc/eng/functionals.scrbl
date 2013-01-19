#lang scribble/doc

@(require (for-label formica))

@(require
  scribble/manual
  scribble/eval)

@(define formica-eval
   (let ([sandbox (make-base-eval)])
     (sandbox '(require formica))
     sandbox))

@title[#:tag "functional"]{Operators and functionals}

@declare-exporting[formica/tools]

The bindings documented in this section are provided by the @racket[formica/tools] library and @racket[formica] language.

@defproc[(function? [x Any]) Bool]
Returns @racket[#t] only if @racket[_x] is a function and @racket[#f] otherwise.


@defproc[(id (x Any)) Any]
The identity function.

Examples:
@interaction[#:eval formica-eval
  (id 1)
  (id 'x)
  (id +)
  ((id +) 1 2)
  (id id)
  (id '(a b c))]


@defproc[(arg (n Index)) Fun]
Creates a trivial function which returns the @racket[_n]-th argument.

Examples:
@interaction[#:eval formica-eval
  (arg 1)
  ((arg 1) 'x 'y 'z)
  ((arg 2) 'x 'y 'z)
  ((arg 3) 'x 'y 'z)]

@defthing*[([I1 (arg 1)]
            [I2 (arg 2)]
            [I3 (arg 3)])]

Aliases for frequently used @racket[arg] calls.

@defproc[(const (x Any)) Fun]
Creates a trivial function which returns @racket[_x] for any arguments.

Examples:
@interaction[#:eval formica-eval
  (const 'A)
  ((const 'A) 1)
  ((const 'A) 1 2)
  ((const +))]


@defproc[(composition [f Fun] ...) Fun]
@defthing[∘ composition]
Returns a generalized composition of functions @racket[_f ...]. 
Function may have any arity: composition is done as if they all were curried.

In the generalized composition variadic or polyadic functions have minimal possible arity,
unless they are @elemref["greedy"]{greedy}.

Function @racket[composition] has an alias: @racket[∘]
(could be entered as @litchar{\circ} + Alt @litchar{\}).

Examples:
@interaction[#:eval formica-eval
  (define-formal (u 1) (b 2) (t 3))
  ((∘ u b) 1 2)
  ((∘ b u) 1 2)
  ((∘ t b u) 1 2 3 4)
  ((∘ t u b) 1 2 3 4)
  ((∘ u t b) 1 2 3 4)
  ((∘ u b t) 1 2 3 4)
  ((∘ b u t) 1 2 3 4)
  ((∘ b t u) 1 2 3 4)
  ((∘ length (filter odd?)) '(1 2 3 4))]

Composition with nullary function:
@interaction[#:eval formica-eval
  (define-formal (n 0) (u 1) (b 2))
  ((∘ u n))
  ((∘ b n) 1)
  ((∘ n u) 1)]

Composition is associative:
@interaction[#:eval formica-eval
  ((∘ (∘ b t) u) 1 2 3 4)
  ((∘ b (∘ t u)) 1 2 3 4)]

Composition has left and right neutral element:
@interaction[#:eval formica-eval
  ((∘ b id) 1 2)
  ((∘ id b) 1 2)]


In the generalized composition variadic or polyadic functions have minimal possible arity:
@interaction[#:eval formica-eval
  (define (f . x) (cons 'f x))
  (define (g x . y) (list* 'g x y))
  ((∘ f g) 1 2 3 4)]

@defproc[(greedy [f Fun]) Fun]
@elemtag["greedy"]{}For variadic or polyadic function @racket[_f] returns equivalent function having 
maximal possible arity.

Examples:
@interaction[#:eval formica-eval
  (define-formal f g h)
  ((∘ f g h) 1 2 3 4)
  ((∘ f (greedy g) h) 1 2 3 4)
  ((∘ remove-duplicates (greedy append)) '(1 2 3) '(2 3 2 4))]


@deftogether[[
 @defproc[(negated (p Fun)) Fun]
  @defthing[#:kind "alias" ¬ negated]]]
Returns the negation of a predicate @racket[_p].

For function @racket[negated] there is an alias: @racket[¬]
(could be entered as @litchar{\neg} + Alt @litchar{\}).

Examples:
@interaction[#:eval formica-eval
  (negated odd?)
  ((negated odd?) 2)
  (define ≥ (¬ <))
  (map ≥ '(1 2 3) '(3 2 1))]


@defproc[(flipped [f Fun]) Fun]
Returns a function which is functionally equivalent to @racket[_f], 
but gets it's arguments in reversed order.

Examples:
@interaction[#:eval formica-eval
  (define snoc (flipped cons))
  snoc
  (snoc 1 2)
  ((flipped list) 1 2 3)]


@defproc[(fif [p Fun] [f Fun] [g Fun]) Fun]
Returns function @centered[@tt{x y ...  = (if (p x y ...) (f x y ...) (g x y ...)).}]

Examples:
@interaction[#:eval formica-eval
  (map (fif odd? (+ 1) id) '(0 1 2 3 4))]


@defproc[(andf [f Fun] [g Fun] ...) Fun]
Returns function @centered[@tt{x y ...  = (and (f x y ...) (g x y ...) ...).}]

Examples:
@interaction[#:eval formica-eval
  (map (andf integer? positive?) '(-3/5 -1 0 2 4.2))]


@defproc[(orf [f Fun] [g Fun] ...) Fun]
Returns function @centered[@tt{x y ...  = (or (f x y ...) (g x y ...) ...).}]

Examples:
@interaction[#:eval formica-eval
  (map (orf integer? positive?) '(-3/5 -1 2 4.2))]

@deftogether[[
 @defproc[(argmap [f Fun] [g unary?]) Fun]
  @defthing[#:kind "alias" /@ argmap]]]
Returns function @centered[@tt{x y ...  = (f (g x) (g y) ...).}]
This function has an alias @racket[/@].

Examples:
@interaction[#:eval formica-eval
  ((argmap cons sqr) 2 3)
  ((/@ + sqr) 1 2 3)]


@defproc[(all-args [p unary?]) Fun]
Returns @centered[@tt{(argmap and p).}]

Examples:
@interaction[#:eval formica-eval
  ((all-args Real) 2 -3 4.5)
  ((all-args Real) 2 'x 4.5)]


@defproc[(any-args [f unary?]) Fun]
Returns function @centered[@tt{(argmap or p).}]

Examples:
@interaction[#:eval formica-eval
  ((any-args Real) '(1 2) 'a "abc" 1-2i)
  ((any-args Real) 'x 2 "abc" 0+8i)]


@defproc*[([(curry [f Fun] [arg Any] ...) (or/c curried? Any)]
            [(curried [f Fun] [arg Any] ...) (or/c curried? Any)])]
Return partially applied (curried) function @racket[_f], with fixed arguments @racket[_arg ...]. Symbols @racket[curry] and @racket[curried] are synonyms.

@defproc*[([(curryr [f Fun] [arg Any] ...) (or/c curried? Any)]
            [(r-curried [f Fun] [arg Any] ...) (or/c curried? Any)])]
Like @racket[curry] but arguments @racket[_arg ...] are fixed from the right side of argument sequence. Symbols @racket[curryr] and @racket[r-curried] are synonyms.

Examples of partial application:
@interaction[#:eval formica-eval
  (curry list 1 2)
  ((curry list 1 2) 3 4)
  (map (curried cons 1) '(1 2 3))
  (curryr list 1 2)
  ((curryr list 1 2) 3 4)
  (map (r-curried cons 1) '(1 2 3))
  (curry cons 1 2)
  (curryr cons 1 2)]

The @racket[curry] (@racket[curried]) and @racket[curryr] (@racket[r-curried]) functions correctly reduce the arity of curried function:
@interaction[#:eval formica-eval
  (procedure-arity cons)
  (procedure-arity (curried cons))
  (procedure-arity (curry cons 1))
  (procedure-arity (curryr cons 1))
  (procedure-arity (curry + 1 2 3))]

@defproc[(curried? [x Any]) Bool]
Returns @racket[#t] if @racket[x] is partially applied or curried function, and @racket[#f] otherwise.

Examples:
@interaction[#:eval formica-eval
  (curried? (curry cons 1))
  (curried (curry +))
  (curried (curryr +))
  (curried? +)]

@defproc[(fixed-point [f Fun] [#:same-test same? (Any Any → Bool) equal?]) Fun]
Returns a function @centered[@tt{x  = (f (f (f ... (f x))))}]
which finds a least fixed point of @racket[_f] by iterative application,
while result keeps changing in the sense of the @racket[_same?] function.

If function @racket[_f] is not unary, it must return as many values, as it could accept.

Example:
@interaction[#:eval formica-eval
  (define fcos (fixed-point cos))
  (fcos 1)
  (cos (fcos 1))]

Let's define naive implementations of secant, bisection and Newton's methods for numerical solution of equation @math{F(x) = 0}.
@defs+int[#:eval formica-eval
  ((define (bisection f)
     (λ (a b)
       (let ([c (* 0.5 (+ a b))])
         (if (<= (* (f a) (f c)) 0)
             (values a c)
             (values c b)))))
   
   (define (secant f) 
     (λ (x y)
       (let ([fx (f x)] 
             [fy (f y)])
         (values y (/ (- (* x fy) (* y fx)) 
                      (- fy fx))))))
     
  (define (newton f) 
    (λ (x)
      (let* ([ε (tolerance)]
             [fx (f x)]
             [dfx (/ (- (f (+ x ε)) fx) ε)])
        (- x (/ fx dfx))))))]

Note that functions @racketidfont{bisection} and @racketidfont{secant} require and produce two approximation points on each step, while @racketidfont{newton} function accepts only one point.

Now we can define the universal function @racketidfont{find-root} which solves given equation using these methods:
@def+int[#:eval formica-eval
   (define (find-root f #:method (method secant))
     (fixed-point (method f) #:same-test almost-equal?))

  (parameterize ([tolerance 1e-10])
    ((find-root (λ (x)(- (cos x) x))
                #:method secant) 0. 1.))
                                                  
  (parameterize ([tolerance 1e-10])
    ((find-root (λ (x)(- (cos x) x)) 
                #:method bisection) 0. 1.))
                                           
  (parameterize ([tolerance 1e-10])
    ((find-root (λ (x)(- (cos x) x)) 
                #:method newton) 1.))]

The @racketidfont{find-root} function inherits arity of it's methods:
@interaction[#:eval formica-eval
 (procedure-arity 
  (find-root (λ (x)(- (cos x) x))
             #:method secant))
 
 (procedure-arity 
  (find-root (λ (x)(- (cos x) x)) 
             #:method bisection))
 
 (procedure-arity 
  (find-root (λ (x)(- (cos x) x)) 
                #:method newton))]