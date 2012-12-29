#lang scribble/doc

@(require (for-label formica))

@(require
  scribble/manual
  scribble/eval)

@(define formica-eval 
   (let ([sandbox (make-base-eval)])
     (sandbox '(require formica))
     sandbox))


@title[#:style '(toc) #:date "2012" #:version "0.0.1"]{Formica: The "functional-oriented" language.}

@author{Sergey B. Samoylenko}

@smaller{Refference guide.}

@defmodulelang[formica]{This guide describes functions and syntax forms defined 
         in the @racket[formica] module.
         
         Formica is available as both a language level and a module that can be used in other languages.}

@include-section["utils.scrbl"]

@local-table-of-contents[]