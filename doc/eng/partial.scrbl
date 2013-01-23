#lang scribble/doc

@(require (for-label formica))

@(require
  scribble/manual
  scribble/eval)

@(define formica-eval
   (let ([sandbox (make-base-eval)])
     (sandbox '(require formica))
     sandbox))

@title[#:tag "partial"]{Partial function application}

@declare-exporting[formica/partial-app]

The bindings documented in this section are provided by the @racket[formica/partial-app] library and @racket[formica] language.

In order to get Formica language without syntax for partial application, use @para[@tt{#lang formica/regular-app}] at the header of the file or @racket[(require formica/regular-app)]. It will load all bindings from  @racket[formica] language except for those provided in @racket[formica/partial-app] library.

One feature which makes @emph{Formica} different from @emph{Racket}, is simplified syntax for @deftech{partial application}, which is close to @emph{Haskell}'s sections.

For example, function @racket[cons], expects two arguments:
@interaction[#:eval formica-eval
  (cons 1 2)]

We may consider it as curried function: as a sequence of nested closures:
@interaction[#:eval formica-eval
  (cons)
  ((cons) 1)
  (((cons) 1) 2)]

Here is partial application of binary function @racket[cons]:
@interaction[#:eval formica-eval
  (cons 1)
  ((cons 1) 2)]
In the expression @racket[(cons 1)] the first argument is fixed, resulting in an unary function.

The partially apllied function becomes a regular function which could be applied, returned as a result or passed as a n argument to another function.
@interaction[#:eval formica-eval
  ((+ 1) 3)
  (define inc (+ 1))
  (inc 3)
  (map (+ 1) '(1 2 3))] 

The simplified syntax makes possible only "left" sections by fixing arguments from left to right. 
For fixing the sequence of arguments "from the right" one has to use explicit partial application using 
@racket[curryr] function.
                       
Examples of explicit partial application:
@interaction[#:eval formica-eval
  (- 1)
  (curry - 1)
  ((curry - 1) 3)
  (map (curry - 1) '(1 2 3))
  (curryr - 1)
  ((curryr - 1) 3)
  (map (curryr - 1) '(1 2 3))]

@defproc[(apply [f Fun] [v Any] ...) Any]
Applies function @racket[_f] to a list of arguments @racket[_v ...]. If the number of arguments is less then arity of function @racket[_f] returns partially applied function.

@interaction[#:eval formica-eval
  (apply cons '(1 2))
  (apply cons '(1))
  (apply cons '(1 2 3))]

The syntax for partial application may make compilation time of a program be longer then usual. However, usually it does not affect the run-time efficiency.