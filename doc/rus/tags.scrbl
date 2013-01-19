#lang scribble/doc

@(require (for-label formica))

@(require
  scribble/manual
  scribble/eval)

@(define formica-eval
   (let ([sandbox (make-base-eval)])
     (sandbox '(require formica/tags))
     sandbox))

@title[#:tag "tags"]{Помеченные функции}

@defmodule[formica/tags]

Библиотка @racketmodname[formica/tags] предоставляет инструменты для 
создания функций помеченных заданным символом.

Это позволяет отличать функции друг от друга и указывать на их свойства.

@defproc[(tagged? [f Fun]) tagged?]
Возвращает @racket[#t], если @racket[_x] является помеченной функцией,
                              и @racket[#f] в противном случае.

@defproc[(tag? [f tagged?]) Sym]
Возвращает метку помеченной функции @racket[_f].
                              
@defproc[(check-tag [f tagged?] [t Any]) Bool]
Возвращает @racket[#t], если @racket[_f] является помеченной функцией,
и имеет метку @racket[_t], в противном случае -- @racket[#f].

@defproc*[[[(set-tag [t Sym]) (-> Fun tagged?)]
           [(set-tag [t Sym] [n (or/c Sym #f)]) (Fun → tagged?)]]]
Возвращает оператор, помечающий функцию меткой @racket[_t].
Если указано имя @racket[_n], то функция будет переименована.

@defproc*[[[(set-tag* [t Sym]) (-> Fun Fun)]
           [(set-tag* [t Sym] [n (or/c Sym #f)]) (Fun → Fun)]]]
Возвращает оператор, добавляющий к имени функции метку @racket[_t]. 
При этом переименованная функция не является помеченной.
Если указано имя @racket[_n], то функция будет переименована.

Примеры:
@interaction[#:eval formica-eval
  (define t-cons ((set-tag 'tag) cons))
  t-cons
  (tag t-cons)
  (check-tag 'tag t-cons)
  (check-tag 'tag cons)
  (tagged? t-cons)]

@interaction[#:eval formica-eval
  (define r-cons ((set-tag* 'tag) cons))
  r-cons
  (check-tag 'tag r-cons)
  (tagged? r-cons)]

@;@(include-extracted "../tags.rkt")