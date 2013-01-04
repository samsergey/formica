#lang scribble/doc

@(require (for-label formica))

@(require
  scribble/manual
  scribble/eval)

@(define formica-eval
   (let ([sandbox (make-base-eval)])
     (sandbox '(require formica))
     sandbox))

@title[#:tag "syntax"]{Syntax features}

@include-section["partial.scrbl"]

@include-section["tacit.scrbl"]