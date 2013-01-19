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

@title[#:tag "control"]{Управление потоком вычислений}

@defform[(begin expr ...)]
Вычисляет последовательность выражений @racket[_expr ...] и возвращает значение последнего из них.

Имеет смысл только для функций с побочным действием.

Примеры:
@interaction[#:eval formica-eval
  (begin
    (+ 1 2)
    (+ 3 4))
  (begin
    (displayln "some text")
    (+ 3 4))]

@section[#:tag-prefix "control"]{Операторы ветвления и выбора}

@defform[(if test-expr then-expr else-expr)]
Оператор ветвления.
Вычисляет @racket[_then-expr], если  @racket[_test-expr] возвращает @racket[#t] или 
любое значение, отличное от @racket[#f], и @racket[_then-expr] --- в противном случае.

Примеры:
@interaction[#:eval formica-eval
  (if #t 2 3)
  (if #f 2 3)
  (if 1 2 3)
  (if #f (/ 0) 3)]

@defform[(cond [test-expr res-expr] ... 
               [else else-expr])]
Оператор выбора.
Вычисляет выражение @racket[_res-expr], соответствующее первому @racket[_test-expr], возвращающему @racket[#t] или 
любое значение, отличное от @racket[#f], и @racket[_else-expr] --- если не 
выполняется ни одно из условий @racket[_test-expr].

Примеры:
@interaction[#:eval formica-eval
  (cond
    [(odd? 2) 2]
    [(odd? 3) 3]
    [else 4])
  (cond
    [(odd? 2) (/ 0)]
    [(odd? 3) 3]
    [else (/ 0)])]


@defform[(or expr ...)]
Оператор дизъюнкции. 
По очереди вычисляет выражения @racket[_expr ...], и, как только одно из них возвращает @racket[#t] или 
любое значение, отличное от @racket[#f], прекращает вычисления, возвращая это значение.
Возвращает @racket[#f], если все выражения @racket[_expr ...] возвращают @racket[#f].

Примеры:
@interaction[#:eval formica-eval
 (or 1 2)
 (or 1 (/ 0) (log -2))
 (or #f #f 4)]
  
Может использоваться, как бинарная функция.
@interaction[#:eval formica-eval
 or
 (map or '(#t #f #t #f) '(#t #t #f #f))
 (foldr (∘ or odd?) #f '(2 4 5 6 8))]


@defform[(and expr ...)]
Оператор конъюнкции. 
По очереди вычисляет выражения @racket[_expr ...], и, как только одно из них возвращает @racket[#f],
прекращает вычисления, возвращая @racket[#f].
Возвращает значение последнего выражения @racket[_expr ...], если все выражения возвращают @racket[#t], 
или любое значение, отличное от @racket[#f].

Примеры:
@interaction[#:eval formica-eval
 (and 1 2)
 (and #f (/ 0) (log -2))
 (and #t #t 4)]
  
Может использоваться как бинарная функция.
@interaction[#:eval formica-eval
 and
 (map and '(#t #f #t #f) '(#t #t #f #f))
 (foldr (∘ and even?) #t '(2 4 6 8))]

@defform[(==> expr1 expr2)]
Оператор импликации. Эквивалентен форме @centered{@tt{(if expr1 expr2 #t)}.}
Может использоваться, как бинарная функция.

@interaction[#:eval formica-eval
  ==>
  (==> 'A 'B)
  (==> #f (/ 1 0))]

@section[#:tag-prefix "control"]{Управление вычислением}

@defform[(quote expr)]
Оставляет выражение @racket[_expr] не вычисленным.
Может быть введена с помощью кавычки @litchar{'}.

Примеры:
@interaction[#:eval formica-eval
 (quote x)
 'x
 '(+ 1 2)]

@deftogether[
[@defform[(quasiquote expr)]
  @defform[(unquote expr)]
  @defform[(unquote-splicing expr)]]]
Оставляет выражение @racket[_expr] не вычисленным, но позволяет вычислить части выражения, 
отмеченные формами @racket[unquote] (@litchar{,}) и @racket[unquote-splicing] (@litchar{,@"@"}).
Может быть введена с помощью обратной кавычки @litchar{`}.

Примеры:
@interaction[#:eval formica-eval
 (quasiquote x)
 `x
 `(+ 1 2)
 `(* (+ 1 2) ,(+ 1 2))
 `(a b ,(list 'c 'd) e)
 `(a b ,@(list 'c 'd) e)]

@defproc*[[((eval [expr Any]) any)
          ((eval [expr Any] [ns namespace?]) any)]]
Вычисляет выражение, заданное с помощью форм @racket[quote] и @racket[quasiquote], используя 
текущее пространство имён или пространство @racket[_ns].

Является интерпретатором языка Formica.

Пространство имён необходимо указывать явно, при использовании функции @racket[eval]
в программе, а не в диалоге с интерпретатором. При этом для использования связываний,
определённых библиотеками, используется функция @racket[make-base-namespace], а для
использования определений, данных в программе --- форма @racket[define-namespace-anchor].

@section[#:tag-prefix "control"]{Отложенные вычисления}

@defform[(delay expr)]
Задерживает вычисление @racket[_expr]. Вычисление может быть 
произведено с помощью функции @racket[force], при этом вычисленное значение мемоизируется.

@defproc[(force [x Any] ...) Any]
Выполняет вычисления, задержанные с помощью @racket[delay].

@interaction[#:eval formica-eval
 (define f (delay (begin (display "evaluating ") 42)))
 f
 (force f)
 (force f)]

О дополнительных формах для задержанных вычислений можно узнать из документации к модулю 
@racket[racket/promise].