#lang scribble/doc

@(require (for-label formica))

@(require
  scribble/manual
  scribble/eval)

@(define formica-eval
   (let ([sandbox (make-base-eval)])
     (sandbox '(require formica))
     sandbox))

@title[#:tag "funs"]{Функции и операторы языка}

В этом разделе приводятся функции и операторы, определённые в языке.

@local-table-of-contents[]

@include-section["equivalence.scrbl"]

@include-section["numeric.scrbl"]

@include-section["pairs.scrbl"]

@include-section["functionals.scrbl"]

@include-section["io.scrbl"]
