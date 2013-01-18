#lang scribble/doc

@(require (for-label formica))

@(require
  scribble/manual
  scribble/eval)

@(define formica-eval
   (let ([sandbox (make-base-eval)])
     (sandbox '(require formica))
     sandbox))

@title[#:tag "utils"]{Miscellaneous issues}

@include-section["differences.scrbl"]

@include-section["comparison.scrbl"]

@include-section["functionals.scrbl"]

@include-section["memoize.scrbl"]

@include-section["tags.scrbl"]

@include-section["arity.scrbl"]

@include-section["testing.scrbl"]