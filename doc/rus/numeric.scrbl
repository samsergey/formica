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

@title[#:tag "numeric"]{Числовые функции}

@section[#:tag-prefix "numeric"]{Отношения порядка}

@defproc[(= [x Num] [y Num] ...) Bool]
Возвращает @racket[#t], если все аргументы равны, и  @racket[#t] --- в противном случае.

@defproc[(< [x Real] [y Real] ...) Bool]
Возвращает @racket[#t], если все аргументы образуют строго возрастающую последовательность, и  
@racket[#t] --- в противном случае.

@defproc[(<= [x Real] [y Real] ...) Bool]
Возвращает @racket[#t], если все аргументы образуют неубывающую последовательность, и  
@racket[#t] --- в противном случае.

@defproc[(> [x Real] [y Real] ...) Bool]
Возвращает @racket[#t], если все аргументы образуют строго убывающую последовательность, и  
@racket[#t] --- в противном случае.

@defproc[(>= [x Real] [y Real] ...) Bool]
Возвращает @racket[#t], если все аргументы образуют невозрастающую последовательность, и  
@racket[#t] --- в противном случае.


@section[#:tag-prefix "numeric"]{Арифметические операции}

@defproc[(+ [x Num] [y Num] ...+) Num]
Вычисляет сумму всех аргументов.

@defproc[(- [x Num] ...) Num]
Вычисляет разность первого аргумента и всех последующих.

Для единственного аргумента @math{x} возвращает @math{--x}.

@defproc[(* [x Num] [y Num] ...+) Num]
Вычисляет произведение всех аргументов.

@defproc[(/ [x Num] ...) Num]
Вычисляет отношение первого аргумента и всех последующих.

Для единственного аргумента @math{x} возвращает @math{1/x}.

@defproc[(expt [x Num] [y Num]) Num]
Вычисляет @math{x^y}.


@section[#:tag-prefix "numeric"]{Прочие функции}

@defproc[(positive? [x Real]) Bool]
Возвращает @racket[#t] если @racket[_x] положительно.

@defproc[(negative? [x Real]) Bool]
Возвращает @racket[#t] если @racket[_x] отрицательно.

@defproc[(modulo [a Int] [b Int]) Int]
Возвращает @racket[_a] по модулю @racket[_b].

@defproc[(even? [a Int]) Bool]
Возвращает @racket[#t] если @racket[_a] чётно.

@defproc[(odd? [a Int]) Bool]
Возвращает @racket[#t] если @racket[_a] нечётно.

@defproc[(abs [x Real]) positive?]
Возвращает модуль действительного числа.

@defproc[(real-part [z Num]) Real]
Возвращает действительную часть числа.

@defproc[(imag-part [z Num]) Real]
Возвращает мнимую часть числа.

@deftogether[
[@defproc[(log [z Num]) Num]
  @defproc[(exp [z Num]) Num]]]
Логарифм и экспонента.

@deftogether[
[@defproc[(sin [z Num]) Num]
  @defproc[(cos [z Num]) Num]
  @defproc[(tan [z Num]) Num]]]
Тригонометрические функции.

@deftogether[
[@defproc[(asin [z Num]) Num]
  @defproc[(acos [z Num]) Num]
  @defproc[(atan [z Num]) Num]]]
          
Обратные тригонометрические функции.

О дополнительных числовых функциях можно узнать из документации к модулям
@racket[racket/base] и @racket[racket/math].