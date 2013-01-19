#lang scribble/doc

@(require (for-label formica))

@(require
  scribble/manual
  scribble/eval)

@(define formica-eval 
   (let ([sandbox (make-base-eval)])
     (sandbox '(require formica))
     sandbox))


@title[#:style '(toc) #:date "2013" #:version "1.0"]{Formica: Функционально-ориентированный язык программирования}

@author{С.Б. Самойленко, П.В. Хан}

@smaller{Справочное руководство по функциям и формам диалекта Formica.}

@defmodulelang[formica]{Функции и синтаксические формы, описываемые в этом справочном руководстве, могут быть использованы как в контексте диалекта Formica, устанавливаемого директивой @para[@tt{#lang formica}] так и в контексте других языков и диалектов в качестве библиотечных.}

@include-section["intro.scrbl"]

@include-section["grammar.scrbl"]

@include-section["computation.scrbl"]

@include-section["types.scrbl"]

@include-section["bindings.scrbl"]

@include-section["functions.scrbl"]

@include-section["monads.scrbl"]

@include-section["testing.scrbl"]

@local-table-of-contents[]