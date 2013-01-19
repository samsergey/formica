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

@title[#:tag "memo"]{Мемоизация}

@elemtag["t:memo"]{@emph{Мемоизация} ---  замена повторного вызова
функции от одних и тех же аргументов результатом, однажды полученным для
этих аргументов.}



@defform*[((define/memo (f var ...) body ...)
          (define/memo f (λ (var ...) body ...)))]
Определяет мемоизированную функцию @racket[_f].

Примеры:
@interaction[#:eval formica-eval
  (define/memo (f x) 
    (display x) 
    (* x x))
  (f 2)
  (f 2)
  (f 3)]


@defproc[(memoized [f Fun]) memoized?]
Возвращает мемоизированный вариант функции @racket[f].

Примеры:
@interaction[#:eval formica-eval
  (define g (memoized current-inexact-milliseconds))
  (g)
  (current-inexact-milliseconds)
  (g)]


@defproc[(memoized? [f Fun]) Bool]
Возвращает @racket[#t], если @racket[_f] является мемоизированной функцией
и @racket[#f] --- в любом другом случае.

Примеры:
@interaction[#:eval formica-eval
  (define g (memoized +))
  (memoized? g)
  (memoized? +)
  (g 2 3)]
