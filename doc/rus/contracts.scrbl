#lang scribble/doc

@(require (for-label formica))

@(require 
  (except-in scribble/manual ::)
  scribble/eval)

@(define formica-eval
   (let ([sandbox (make-base-eval)])
     (sandbox '(require formica)) 
     sandbox))

@declare-exporting[formica]

@title[#:tag "contracts"]{Сигнатуры функций}

@elemtag["t:signature"]{@emph{Сигнатура функции}} определяет множество определения и множество значений функции. С другой стороны, в рамках системы типов Formica, сигнатуру можно рассматривать, как @emph{контракт для функции}, то есть совокупность пред- и постусловий. Предусловия представляют собой описание типов для допустимых фактических аргументов функции, а постусловие --- тип возвращаемого результата.

Сигнатуры решают две задачи: 
@itemize{@item{осуществляют динамический контроль типов;}
         @item{являются средством документирования кода.}}

Сигнатуры не осуществляют статический контроль типов.
          
При нарушении сигнатуры, выводится сообщение об ошибке, содержащее следующую информацию:
@itemize{@item{несоответствие типов, приведшее к ошибке;}
         @item{"виновник нарушения" --- модуль который вызвал функцию с нарушением сигнатуры;}
         @item{"обвинитель" --- модуль, который предоставляет функцию с сигнатурой;}
         @item{определение сигнатуры.}}

@section[#:tag "contracts:functions"]{Определение сигнатур}

@defform/subs[#:literals (? ..)
   (-> dom range)
   ([dom (type ...)
         (type ... rest ..)
         (type ... (? opt ...))
         (type ... (? opt ...) rest ..)])]
         
Определяет сигнатуру для функции, имеющую область определения @racket[_dom], 
область значений @racket[_range]. Типы необязательных аргументв перечисляются в форме @racket[(? _opt ...)]. Для вариадических функций тип списка аргументов определяется контрактом @racket[_rest] и следующим за ним ключевым символом @litchar{..} .

Может быть введён в инфиксной форме:

@racketblock[(_dom -> _range)]

@section[#:tag "contracts:declaration"]{Объявление сигнатуры функции}

Сигнатура функции может быть указана либо при её определении, с помощью формы @racket[::],
либо при экспортировании функции, с помощью формы @racket[contract-out].


@defform[(:: f sig definitions ...)]
Связывает сигнатуру @racket[_sig] с функцией @racket[_f], определяемой в одном из выражений
@racket[_definitions ...]. Сигнатура может включать в себя свободные символы,
воспринимаемые, как полиморфные типы.

Определение @racket[_definition] может быть задано любой формой для связывания:
@racket[define], @racket[define/c], @racket[define/.], @racket[define//.] или @racket[define/memo].

@bold{Примеры}

Определение числовой функции:
@def+int[#:eval formica-eval
  (:: sqr (Num -> Num)
   (define (sqr x)
     (* x x)))
   (sqr 3)
   (sqr 'a)]

Определим функцию, требующую, чтобы её первый аргумент
был положительным числом, второй --- символом; при этом
функция должна возвращать чётное число.

@def+int[#:eval formica-eval
  (:: f (positive? Sym -> even?)
   (define (f x s)
     (* 2 x)))
   (f 3 'a)
   (f -3 'a)
   (f 3/2 'a)]

Сигнатура функции @racket[append], как для вариадической функции, принимающей, как минимум, один аргумент. При этом
все аргументы должны быть списками. Возвращает она список:
@def+int[#:eval formica-eval
  (:: append (list? list? .. -> list?)
   (define/. append
     a --> a
     a '() --> a
     a b --> (foldr cons b a)
     a b ... --> (append a (apply append b))))
   (append '(1 2 3) '(a b c) '(x y))
   (append '(a b c))
   (append '(a b c) 5)]

Следующая функция принимает в качестве первого аргумента  
числовую функцию-предикат, а в качестве второго --- список чисел. 
Возвращает она значение логического типа.

@def+int[#:eval formica-eval
   (:: any ((Num -> Bool) (list: Num ..) -> Bool)
     (define/c (any pred)
       (foldr (∘ or pred) #f)))
   (any odd? '(1 2 3 4))
   (any odd? '(0 2 8 4))
   (any + '(0 2 8 4))
   (any odd? '(0 x 8 4))]

Определённая ниже функция имеет два обязательных и один необязательный аргумент:

@def+int[#:eval formica-eval
  (:: range (Real (? Real Real) -> (list: Real ..)) 
    (define/. range
      n --> (range 1 n)
      a b --> (range a b 1)
      a b s --> (let loop ([i a] [res '()])
                  (if (< (- b i (- s)) s)
                      (reverse res)
                      (loop (+ i s) (cons i res))))))
  (range 4)
  (range 3 8)
  (range -1 1 1/2)]
   
@bold{Полиморфные типы в сигнатурах}

Сигнатура следующей функции использует полиморфные типы @racket[A] и @racket[B].

@def+int[#:eval formica-eval
   (:: bind (A (A -> B) -> B)
     (define (bind x f)
       (f x)))
   (bind 4 sqrt)
   (bind 's cons)]

Ниже даётся сигнатура функции @racket[foldr]:

@def+int[#:eval formica-eval
  (:: foldr ((a b -> b) b (list: a ..) -> b)
    (define/c (foldr f x0)
      (/. '() --> x0
          (cons h t) --> (f h (foldr f x0 t)))))
  (foldr (hold f) 'x0 '(1 2 3))
  (foldr + 0 '(1 2 3))]