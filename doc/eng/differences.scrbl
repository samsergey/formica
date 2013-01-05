#lang scribble/doc

@(require (for-label formica))

@(require
  scribble/manual
  scribble/eval)

@(define formica-eval
   (let ([sandbox (make-base-eval)])
     (sandbox '(require formica))
     sandbox))

@title[#:tag "differences"]{Differences from Racket}

@declare-exporting[formica]

In order to use simplified syntax for @tech{partial application} and curring, the arity of some variadic functions, defined in the Racket is changed in Formica.

@defproc*[([(+ [x Num] [y Num] ...+) Num]
           [(* [x Num] [y Num] ...+) Num])]
Same as @racket[+] and @racket[*] defined in @racket[racket/base], but accept at least two arguments.

@interaction[#:eval formica-eval
  (+ 1 2 3)
  (+ 1 2)
  (+ 1)
  (+)
  (map (+ 1) '(1 2 3))
  (* 1)
  (map (* 2) '(1 2 3))]

@defproc[(map [f Fun] [lsts list?] ...+) list?]
Same as @racket[map], defined in @racket[racket/base], but accepts at least one list.

@interaction[#:eval formica-eval
  (map cons '(1 2 3) '(a b c))
  (map (* 2) '(1 2 3))
  (map (* 2))
  (map (map (* 2)) '((1 2 3) (2 3 4) (5)))]

@defproc*[([(all? [f Fun] [lsts list?] ...+) Bool]
           [(any? [f Fun] [lsts list?] ...+) Bool])]
Same as @racket[andmap] and @racket[ormap] defined in @racket[racket/base], but accept at least one list.

@interaction[#:eval formica-eval
  (any? odd? '(1 2 3))
  (all? < '(1 2 3) '(2 3 4))]

Some forms could be used as functions

@defform[(or expr ...)]
@defform[(and expr ...)]
Same as @racket[or] and @racket[and] forms defined in @racket[racket/base], however, evaluate to binary boolean functions if used not in a head position of application form.

@interaction[#:eval formica-eval
  or
  and
  (or 1 (/ 1 0))
  (and #f (/ 1 0))
  (map or '(#t #f #t #f) '(#t #t #f #f))
  (define/c (any p) (foldr (∘ or p) #f))
  (define/c (all p) (foldr (∘ and p) #f))
  (any odd? '(2 4 6 5 8 10))
  (all even? '(2 4 6 5 8 10))]

@defform[(==> expr1 expr2)]
Represents implication. Equivalent to @centered{@tt{(if expr1 expr2 #t)}.}
Returns binary boolean function if used not in a head position of application form.

@interaction[#:eval formica-eval
  ==>
  (==> 'A 'B)
  (==> #f (/ 1 0))]

@defproc*[([(eq? [x Any] [y Any] ...+) boolean?]
           [(equal? [x Any] [y Any] ...+) boolean?])]
Same as @racket[eq?] and @racket[equal?] provided @racket[racket/base], but if more then two arguments are given, arguments are compared pairwise from left to right.