#lang scribble/doc

@(require (for-label formica))

@(require
  scribble/manual
  scribble/eval)

@(define formica-eval
   (let ([sandbox (make-base-eval)])
     (sandbox '(require formica))
     sandbox))

@title[#:tag "tags"]{Tagged functions}

@declare-exporting[formica]

The bindings documented in this section are provided by the @racket[formica/tools] library and @racket[formica] language.

Tagging allows to distinguish functions and classes of functions.

@defproc[(tagged? [f Fun]) tagged?]
Returns @racket[#t] if @racket[_f] is tagged function, and @racket[#f] otherwise.

@defproc[(tag [f tagged?]) Sym]
Returns a tag of a tagged function @racket[_f].
                              
@defproc[(check-tag [f tagged?] [t Any]) Bool]
Returns @racket[#t] if @racket[_f] is a tagged function and has tag @racket[_t], otherwise returns @racket[#f].

@defproc[(set-tag [t Sym] [n (or/c Sym #f) #f]) (Fun → tagged?)]
Returns an operator, which sets a tag  @racket[_t] to given function.
If name @racket[_n] is given, tagged function will be renamed.

@defproc[(set-tag* [t Sym] [n (or/c Sym #f) #f]) (Fun → Fun)]
Returns an operator, which adds symbol @racket[_t] to function name. 
The result is not a tagged function. If name @racket[_n] is given, the function will be renamed.

Examples:
@interaction[#:eval formica-eval
  (define t-cons ((set-tag 'tag) cons))
  t-cons
  (tag t-cons)
  (check-tag 'tag t-cons)
  (check-tag 'tag cons)
  (tagged? t-cons)]

@interaction[#:eval formica-eval
  (define r-cons ((set-tag* 'tag) cons))
  r-cons
  (check-tag 'tag r-cons)
  (tagged? r-cons)]