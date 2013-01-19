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

@title[#:tag "formal"]{Формальные функции}

@elemtag["t:formal"]{@emph{Формальными}} называются функции, которые, не производя никаких вычислений, возвращают выражение, обозначающее их аппликацию, т.н. @elemtag["t:formal-application"]{@emph{формальную аппликацию}}.

@centered[@racket[(f 'x 'y)  ==>  '(f x y)]]

Формальная аппликация представляет собой список, первым элементом которого является имя формальной функции, а прочие элементы являются ее фактическими  аргументами.

Формальные аппликации могут быть распознаны с помощью @elemref["t:patt"]{шаблонов} в подстановках и могут быть использованы для создания @elemref["t:ADT"]{абстрактных типов данных}.

@section[#:tag "formal applications"]{Определение формальных функций}


@defform/subs[(define-formal f-spec ...) 
([f-spec
   id
   (id arity-spec)])]

Определяет формальные функции, заданные спецификациями @racket[_f-spec ...]. При этом 
символ @racket[_id] определеяет имя формальной функции, а @racket[_arity-spec] -- её валентность.

При определении формальной функции @racket[f], создаются две предикатных функции:
@itemize{@item{@racket[f?], возвращающая @racket[#t], если её аргумент является формальной аппликацией функции @racket[f].}
         @item{@racket[(f: _type ...)], контракт для аппликации формальной функции @racket[f] к аргументам, имеющим типы @racket[_type ...].}}

Примеры:
@interaction[#:eval formica-eval
  (define-formal f)
  f
  (f 2)
  (f 'x 'y)
  (map f '(a b c) '(x y z))
  (foldr f 'x0 '(a b c))
  (f? (f 1 2))
  (f? '(1 2))
  (is (f 1) (f: Num))
  (is (f 1 'x) (f: Num Sym))
  (is (f 1 2 3) (f: Num ..))]

Формальная функция с явно заданной валентностью:

@interaction[#:eval formica-eval
  (define-formal (g 2))
  g
  (g 2)
  (g 'x 'y)
  (g 1 2 3)
  (map g '(a b c) '(x y z))]

Использование в качестве шаблонов:

@interaction[#:eval formica-eval
  (define-formal f (g 2))
  ((/. (f x) --> x) (f 5))
  ((/. (f x y ...) --> y) (f 1 2 3 4))
  ((/. (g a b) --> (list a b)) (g 1 2))
  ((/. (g a) --> a) (g 1 2))]


@deftogether[(
              @defproc[(hold [f (or/c Fun Sym)] [arity procedure-arity? (arity-at-least 0)]) formal-function?]
              @defthing[#:kind "alias" $ hold])]
Возвращает формальную функцию @racket[_f]. При этом можно явно указать валентность @racket[_arity]
формальной функции. 

Для формы @racket[hold] существует сокращённая форма записи: @racket[$].  

Примеры:
@interaction[#:eval formica-eval
  (hold +)
  (map + '(1 2 3) '(10 20 30))
  (map ($ +) '(1 2 3) '(10 20 30))
  (foldr ($ +) 'x0 '(a b c))]

@interaction[#:eval formica-eval
  ($ f 2)
  (($ f 2) 1)
  ((($ f 2) 1) 2)
  (($ f 2) 1 2)
  (($ f 2) 1 2 3)]

@defproc[(formal-function? [x Any]) Bool]
Возвращает @racket[#t], если @racket[_x] является формальной функцией, и @racket[#f] -- в противном случае.

Примеры:
@interaction[#:eval formica-eval
  (define-formal f)
  (formal-function? f)
  (formal-function? +)
  (formal-function? (hold +))]

@section{Идентификация формальных аппликаций}


@defproc[(formal? [v Any]) Bool]
Возвращает @racket[#t], если @racket[_v] является формальной аппликацией.

Примеры:
@interaction[#:eval formica-eval
  (define-formal f)
  (formal? (f 1 2))
  (formal? '(+ 1 2))]

@defform[(formal patt)]
Шаблон, соответствующий любой формальной аппликации. Позволяет идентифицировать и именовать не только аргументы формальной функции, но и саму функцию.

Примеры:
@interaction[#:eval formica-eval
  (define-formal f)
  ((/. (formal (F args ...)) --> `(function: ,F arguments: ,args))
   '(f 1 2))
     
  ((/. (formal (F args ...)) --> `(function: ,F arguments: ,args))
   '(F 1 2))]


@defproc[(n/f-list? [v Any]) Bool]
Возвращает @racket[#t], если @racket[_v] является списком, но не формальной аппликацией.
