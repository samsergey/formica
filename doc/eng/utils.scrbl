#lang scribble/doc

@(require (for-label formica))

@(require
  scribble/manual
  scribble/eval)

@(define formica-eval
   (let ([sandbox (make-base-eval)])
     (sandbox '(require formica 
                        formica/tags))
     sandbox))

@title[#:tag "utils"]{Different utilities}

@include-section["tags.scrbl"]

@include-section["arity.scrbl"]