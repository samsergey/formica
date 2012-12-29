#lang scribble/doc

@(require (for-label formica))

@(require
  scribble/manual
  scribble/eval)

@(define formica-eval 
   (let ([sandbox (make-base-eval)])
     (sandbox '(require formica))
     sandbox))


@title[#:style '(toc) #:date "2012" #:version "1"]{Formica: Функционально-ориентированный язык программирования}

@author{С.Б. Самойленко, П.В. Хан}

@smaller{Справочное руководство по функциям и формам диалекта Formica.}

@defmodulelang[formica]{Функции и синтаксические формы, описываемые в этом справочном руководстве, 
         определены в модуле  @racket[formica]. Они могут быть использованы как в контексте диалекта Formica,
         устанавливаемого директивой @tt{#lang}, так и в контексте других языков и диалектов в качестве библиотечных.}


@include-section["tags.scrbl"]

@local-table-of-contents[]