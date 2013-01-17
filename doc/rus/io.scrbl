#lang scribble/doc

@(require (for-label formica))

@(require
  scribble/manual
  scribble/eval)

@(define formica-eval
   (let ([sandbox (make-base-eval)])
     (sandbox '(require formica))
     sandbox))

@;@declare-exporting[formica]

@title[#:tag "io"]{Операции ввода-вывода}

@defproc[(read) any]
Считывает выражение из консоли.

@defproc[(display [expr Any]) void?]
Выводит выражение на консоль.

Примеры:
@interaction[#:eval formica-eval
  (begin
    (display "abcd")
    (display (+ 1 2))
    (display '(+ 1 2)))]

@defproc[(displayln [expr Any]) void?]
Выводит выражение на консоль и переводит строку.

Примеры:
@interaction[#:eval formica-eval
  (begin
    (displayln "abcd")
    (displayln (+ 1 2))
    (displayln '(+ 1 2)))]

@defproc[(printf [format Str] [expr Any] ...) void?]
Выводит форматированное выражение на консоль.

Формат задаётся строкой @racket[_format], где флаг @racket["~a"] означает 
вывод очередного выражения из @racket[_expr ...].

Примеры:
@interaction[#:eval formica-eval
    (printf "abcd ~a efgh ~a \n" 25 '(x y z))]

@defproc[(newline) void?]
Выводит символ перевода строки на консоль.

@defproc[(error [message Str] [expr Any] ...) void?]
Прерывает выполнение программы и выводит заданное сообщение об ошибке @racket[_message].

Примеры:
@interaction[#:eval formica-eval
    (error "This is a fake error!" '(a b c))]

@defproc[(time [expr Any]) any]
Вычисляет выражение @racket[_expr], возвращает его результат и выводит информацию
о времени, затраченном на вычисление

Пример:
@interaction[#:eval formica-eval
  (time (foldr + 0 (range 100000)))]
здесь @racket{cpu time} --- время, затраченное процессором,
 @racket{real time} --- время, затраченное операционной системой,
 @racket{gc time} --- время, затраченное сборщиком мусора. 
Всюду время выводится в миллисекундах.
