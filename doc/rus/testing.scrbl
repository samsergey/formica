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

@title[#:tag "testing"]{Тестирование}


@defform/subs[#:literals (==> =error=>)
 (test tst-spec ...)
 ([tst-spec (expr ==> res)
            (expr =error=> exn?)
            "str"
            expr])]
                                  
Выполняет тестирование, либо сравнивая значение выражения @racket[_expr] со значением выражениея @racket[_res] с помощью @racket[almost-equal?], либо, если вычисление выражения должно приводить к ошибке, проверяет тип возникающего при этом исключения предикатом @racket[_exn?], либо прверяя на истинность выражение @racket[_expr]. Если  в качестве теста указана строка @racket["str"], она используется для именования группы последующих тестов.

Примеры удачных тестов:
@interaction[#:eval formica-eval
 (test
  "простые тесты"
  (- 4 2) ==> 2
  (cons 1 (cons 2 null)) ==> '(1 2)
  (cons 1 (cons 2 3)) ==> '(1 2 . 3)
  "проверка на истинность"
  (odd? 3)
  (member 'x '(x y z))
  "проверка на исключение"
  (car '()) =error=> exn:fail:contract?)]

Пример неудавшегося теста:
@interaction[#:eval formica-eval
 (test
  "успешно пройденный тест"
  (+ 1 1) ==> 2
  "пример не пройденного теста"
  (cons 1 (cons 2 3)) ==> '(1 2 3))]

О дополнительных функциях и формах для тестирования можно узнать из документации к модулю
@racket[rackunit].