#lang scribble/doc

@(require (for-label formica))

@(require
  scribble/manual
  scribble/eval)

@(define formica-eval 
   (let ([sandbox (make-base-eval)])
     (sandbox '(require formica))
     sandbox))

@;@(define book-path (collection-file-path "FLPBook.pdf" "flp/doc"))

@title[#:style '(toc) #:date "2013" #:version "1.0"]{Formica: Функционально-ориентированный язык программирования}

@author{С.Б. Самойленко, П.В. Хан}

@smaller{Справочное руководство по функциям и формам диалекта Formica.}

@defmodulelang[formica]{Функции и синтаксические формы, описываемые в этом справочном руководстве, могут быть использованы как в контексте диалекта Formica, устанавливаемого директивой @tt{#lang}, так и в контексте других языков и диалектов в качестве библиотечных.}

@;@margin-note{Книга @nonbreaking{С.Б. Самойленко,} @nonbreaking{П.В. Хан} 
@;                   @(linebreak)
@;                   @hyperlink[book-path]{Основы функционального программирования в примерах и задачах.}}

Formica является диалектом языка Racket и используется в рамках курса @emph{"Функциональное и логическое программирование"}, (КамчатГТУ).

@include-section["intro.scrbl"]

@include-section["grammar.scrbl"]

@include-section["computation.scrbl"]

@include-section["control.scrbl"]

@include-section["bindings.scrbl"]

@include-section["types.scrbl"]

@include-section["contracts.scrbl"]

@include-section["formal.scrbl"]

@include-section["equivalence.scrbl"]

@include-section["numeric.scrbl"]

@include-section["pairs.scrbl"]

@include-section["memoize.scrbl"]

@include-section["rewrite.scrbl"]

@include-section["functionals.scrbl"]

@include-section["io.scrbl"]

@;@include-section["testing.scrbl"]

@;@include-section["monads.scrbl"]

@;@include-section["synonims.scrbl"]

@local-table-of-contents[]