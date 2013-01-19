#lang scribble/doc

@(require (for-label formica))

@(require
  scribble/manual
  scribble/eval)

@(define formica-eval
   (let ([sandbox (make-base-eval)])
     (sandbox '(require formica))
     sandbox))

@declare-exporting[formica]

@title[#:tag "testing"]{Testing}

@defform/subs[#:literals (==> =error=>)
                         (test tst-spec ...)
                         ([tst-spec (expr ==> res)
                                    (expr =error=> exn?)
                                    "str"
                                    expr])]

A wrapper for @racket[rackunit] testing tools.
Performs testing, either by comparing @racket[_expr] with @racket[_res] using @racket[almost-equal?], or, if evaluation of @racket[_expr] leads to an error, by checking the raised exeption by @racket[_exn?]. If expression @racket[_expr] is given without expected result it is checked by @racket[check-true]. 

If as a test @racket[_tst-spec] a string @racket["str"] is given it is used as a name for a test-case, including following tests.

Examples of passed tests:
@interaction[#:eval formica-eval
 (test
  "simple tests"
  (- 4 2) ==> 2
  (cons 1 (cons 2 null)) ==> '(1 2)
  (cons 1 (cons 2 3)) ==> '(1 2 . 3)
  "checking truth"
  (odd? 3)
  (member 'x '(x y z))
  "checking exeptions"
  (car '()) =error=> exn:fail:contract?)]

Example of failed test:
@interaction[#:eval formica-eval
 (test
  "test which is passed"
  (+ 1 1) ==> 2
  "failed simple test"
  (cons 1 (cons 2 3)) ==> '(1 2 3))]

Any checkers provided by @racket[rackunit] library could be used in the @racket[test] form.

@interaction[#:eval formica-eval
 (test
  "tests from rack-unit"
  (car (cons 2 3)) ==> 2
  (check-pred Sym 'a)
  (check < 2 3))]
