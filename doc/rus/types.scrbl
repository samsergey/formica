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

@title[#:tag "types"]{Cистема типов}

Formica --- язык со @emph{строгой динамической типизацией}.

Типы в языке выполняют идентификационную и охраняющую роль, оптимизации, основанной на типах, в языке нет.

Указывать типы функций и их аргументов не обязательно, однако их использование помогает документировать код, делает его более устойчивым и облегчает отладку
программ.

Тип связан с величиной, а не с переменной, он представляет множество к которому принадлежит величина. Множество может быть задано перечислением, предикатом или  индуктивным определением.

В языке можно использовать
@itemize{@item{@elemref["t:prim"]{простые типы};}
         @item{@elemref["t:ADT"]{абстрактные} алгебраические и полиморфные типы;}
         @item{@elemref["t:funtype"]{функциональные типы} и @elemref["t:signature"]{сигнатуры функций}.}}

@local-table-of-contents[]

@section[#:tag "type:predicates"]{Контракты}

@emph{Типы данных} определяются контрактами.

@elemtag["t:predicate"] В роли контракта может выступать
@itemize{@item{константа, принадлежащая простому типу;}
         @item{любой унарный предикат --- функция, возвращающая @racket[#t] или любое значение отличное от @racket[#f], для величин соответствующего типа и @racket[#f] -- для любых других величин.;}
         @item{составной контракт, сконструированный с помощью @elemref["t:type:comb"]{комбинаторов контрактов}.}}

Контракты можно определять, как обыкновенные функции --- с помощью формы @racket[define].

@defproc[(Type [v Any]) Bool]
Возвращает @racket[#t] только если @racket[_v] может быть
использовано, как контракт, и @racket[#f] --- во всех других случаях.

@interaction[#:eval formica-eval
 (is Num Type)
 (is (andf integer? positive?) Type)
 (is cons Type)]

Любая константа, принадлежащая @elemref["t:prim"]{простому типу}, представляет собой @emph{единичный тип}
и может рассматриваться, как контракт для этого типа.

@interaction[#:eval formica-eval
 (is 5 Type)
 (is 'abc Type)
 (is '(a b c) Type)]


@defform[(is v type-pred) #:contracts ([v Any] [type-pred Type])]
Возвращает @racket[#t], если @racket[_v] принадлежит типу,
определяемому контрактом @racket[_type-pred], и @racket[#f] --- во всех других случаях.

Отличается от непосредственной аппликации предиката тем, что
@itemize{@item{позволяет рассматривать единичные типы;} 
         @item{если аппликация предиката @racket[_type-pred] к величине @racket[_v] приводит к ошибке, возвращает @racket[#f] не останавливая выполнения программы.}}

@interaction[#:eval formica-eval
 (is 'abc Sym)
 (is 'abc 'abc)
 (is 1.2 odd?)]

Непосредственное применение предиката @racket[odd?] к числу, не являющемуся целым,
привело бы к ошибке:
@interaction[#:eval formica-eval
 (odd? 1.2)]

Любая величина может одновременно принадлежать неограниченному множеству типов.
Так, например, число @racket[5] одновременно принадлежит следующим типам:
@itemize{@item{единичному типу @racket[5]:
           @interaction[#:eval formica-eval (is 5 5)]}          
         @item{числовому типу, определяемому предикатом @racket[Num]:
           @interaction[#:eval formica-eval (is 5 Num)]}
         @item{числовому типу "целое число", определяемому предикатом @racket[integer?]:
           @interaction[#:eval formica-eval (is 5 integer?)]}
         @item{числовому типу "нечётное число", определяемому предикатом @racket[odd?]:
           @interaction[#:eval formica-eval (is 5 odd?)]}
         @item{алгебраическому типу, определяемому контрактом @racket[(or/c 0 1 2 3 5 8 13)]:
           @interaction[#:eval formica-eval (is 5 (or/c 0 1 2 3 5 8 13))] и т.д.}}

Все величины принадлежат типу @racket[Any].

Проверка типов может организовываться явно, с помощью управляющих конструкций @racket[if], @racket[cond] и т.п. или же путём определения @elemref["t:signature"]{сигнатур функций}.

@section[#:tag "types:primitive"]{Простые типы данных}


К @elemtag["t:prim"]{простым типам} относятся
@itemize{@item{типы данных, определяемых предикатами (@emph{логический тип}, @emph{числа}, @emph{строки}, @emph{символы} и т.д.),}
         @item{@emph{функциональные типы}.}}

Все функции и подстановки удовлетворяют предикату @racket[function?].

К @elemtag["t:funtype"]{функциональным типам} относятся:
@itemize{@item{@elemref["t:memo"]{мемоизированные} функции, определяемые предикатом @racket[memoized?],}
         @item{@elemref["t:curry"]{каррированные} и @elemref["t:partial"]{частично применённые} функции, определяемые предикатом @racket[curried?],} 
         @item{@elemref["t:formal"]{формальные} функции, определяемые предикатом @racket[formal?],}
         @item{@elemref["t:predicate"]{контракты}, определяемые предикатом @racket[Type].}}

Функциональные типы, описывающие типы для аргументов и возвращаемого значения, можно
определять с помощью @elemref["t:signature"]{сигнатур}.

Некоторые часто используемые простые типы имеют краткие обозначения. Они могут рассматриваться, как имена множеств, отражающие эти типы.


@defthing[Bool Type] определяет множество величин логического типа. Эквивалентен предикату @racket[boolean?].
@defthing[Num Type] определяет множество чиел. Эквивалентен предикату @racket[Num].
@defthing[Real Type]  определяет множество действительных чисел. Эквивалентен предикату @racket[real?].
@defthing[Int Type]  определяет множество целых чисел. Эквивалентен предикату @racket[integer?].
@defthing[Nat Type]  определяет множество натуральных чисел.
@defthing[Index Type]  определяет множество натуральных чисел больших нуля.
@defthing[Str Type]  определяет множество строк. Эквивалентен предикату @racket[string?].
@defthing[Sym Type] определяет множество символов. Эквивалентен предикату @racket[symbol?].
@defthing[Fun Type]  определяет множество функций. Эквивалентен предикату @racket[function?].


@section[#:tag "type:definition"]{Определение типов}

@defform*[[(define-type name)
           (define-type (name dom ...))
           (define-type name c ...)
           (define-type (name x ...) c ...)] #:contracts ([c Type] [x Type])]
Определяет именованные абстрактные, алгебраические или параметризованные типы.
@itemize{@item{@racket[(define-type name)] --- определяет абстрактный тип-контейнер, в виде формальной функции.}
         @item{@racket[(define-type (name dom ...))] --- определяет алгебраический тип, являющийся произведением типов, в виде формальной функции с областью определеня @racket[_dom]. Область определения указывается так же, как и в @seclink["contracts:functions"]{сигнатурах}.}
         @item{@racket[(define-type name c ...)] --- определяет алгебраический тип, являющийся суммой типов @racket[_c ...].}
         @item{@racket[(define-type (name x ...) c ...)] --- определяет параметризованный тип.}}

@subsection[#:tag "ss:type:comb"]{Комбинаторы контрактов}

Тип величины в Formica описывает множество, к которому величина принадлежит. Кроме непосредственного определения перечислением, с помощью предиката или индуктивного, множества можно определять алгебраически: путём их объединения, пересечения или отрицания.  @elemtag["t:type:comb"]{@emph{Комбинаторы контрактов}} позволяют комбинировать контракты друг с другом алгебраически.

@defproc[(Any [v Any]) Type]
контракт для произвольного выражения.

@deftogether[
[@defproc[(∪ [c Type] ...) Type]
 @defproc[(∩ [c Type] ...) Type]
 @defproc[(\\ [c Type]) Type]]]
Объединение, пересечение и дополнение контрактов.

@subsection[#:tag "types:container"]{Контейнерные типы}

Алгебраические типы данных создаются с помощью контейнерных типов: пар или формальных функций.


@defproc[(cons: [c1 Type] [c2 Type]) Type]
контракт для @elemref["t:pair"]{пары}, элементы которой имеют типы @racket[_c1] и @racket[_c2].

@interaction[#:eval formica-eval
  (is (cons 1 2) (cons: 1 2))
  (is (cons 1 2) (cons: Num Num))
  (is (cons 1 'x) (cons: Num Num))
  (is (cons 1 (cons 2 'x)) (cons: Num (cons: Num Sym)))]

@defform[(list: dom)]
контракт для @elemref["t:list"]{списка}, имеющего элементы, принадлежащие области @racket[_dom], которая указывается так же, как область определения в @seclink["contracts:functions"]{сигнатурах}.

Примеры:

@interaction[#:eval formica-eval
 (is '(1 2) (list: 1 2))
 (is '(1 2) (list: Num Num))
 (is '(1 2) (list: Num Sym))
 (is '(1 2 -3) (list: positive? positive? negative?))]

@interaction[#:eval formica-eval
 (is '(1 2) (list: Num ..))
 (is '(1 1 1 1) (list: 1 ..))
 (is '(1 2 x 4 5) (list: Num ..))
 (is '(1 2 30 1) (list: positive? ..))]
                                       
@interaction[#:eval formica-eval
 (is '(1 2 3) (list: 1 Num ..))
 (is '(1 2 2 2) (list: 1 2 ..))
 (is '(1 2 x 4 5) (list: 1 2 Num ..))]

@interaction[#:eval formica-eval
 (is '(1) (list: 1 (? 2 3)))
 (is '(1 2) (list: 1 (? 2 3)))
 (is '(1 2 3) (list: 1 (? 2 3)))
 (is '(1 2 3 4) (list: 1 (? 2 3)))]

@defform/none[(f: dom) #:contracts [(c Type)]]
контракт для аппликации @elemref["t:formal"]{формальной функции} @racket[_f], имеющей область определения @racket[_dom]. Область определения указывается так же как и для контракта @racket[list:].

Полный список определённых в языке контрактов
можно найти в  документации к модулю @racket[racket/contract]. 

@bold{Примеры}

С помощью контрактов можно определять разнообразные специальные типы
Например, тип для натуральных чисел можно определить так
@def+int[#:eval formica-eval
  (define (natural? x)
    (and (integer? x) 
         (positive? x)))
  (is 9 natural?)
  (is -4 natural?)
  (is "abc" natural?)]
или с помощью формы @racket[define-type] и комбинатора @racket[and/c]:
 @def+int[#:eval formica-eval
  (define-type natural? 
    (and/c integer? positive?))
  (is 9 natural?)
  (is -4 natural?)
  (is "abc" natural?)]
 
 
Перечислимый тип "друг":

@def+int[#:eval formica-eval
  (define-type friend?
    "Андрей" "Маша" "Пак Хэ Донг" "Джеймс")
  (is "Маша" friend?)
  (is "Панкрат" friend?)
  (is 845 friend?)]

Индуктивный тип "список":

@def+int[#:eval formica-eval
  (define-type list? 
    null?
    (cons: Any list?))
  (is '(a b c) list?)
  (is (cons 'a (cons 'b (cons 'c '()))) list?)
  (is (cons 'a (cons 'b 'c)) list?)
  (is 42 list?)]

Индуктивный параметризованный тип: "список величин типа A":
@def+int[#:eval formica-eval
  (define-type (listof A)
    null?
    (cons: A (listof A)))
  (is '(1 2 3) (listof integer?))
  (is '(1 2 x) (listof integer?))
  (is '(a b c) (listof Sym))]

@subsection[#:tag "types:ADT"]{Абстрактные типы}

@elemtag["t:ADT"]{@emph{Абстрактным типом данных}} будем называть комбинацию данных других типов (абстрактных или простых), объединённую с помощью @emph{контенерного типа}. 

Доступ к данным, заключённым в абстрактный тип, осуществляется либо с помощью функций-селекторов, либо с помощью шаблонов и механизма сопоставления с образцом.

Примером абстрактного типа является @elemref["t:pair"]{точечная пара}.

@bold{Пример 1.}

Определим с помощью формальной функции абстрактный тип, эквивалентный 
точечной паре:

@def+int[#:eval formica-eval
  (define-type (kons Any Any))
  (kons 1 2)
  (kons (kons 1 2) (kons 3 4))]

При определении формальной функции, создаётся соответствующий ей контракт:

@interaction[#:eval formica-eval
  (is (kons 1 2) kons?)
  (is (cons 1 2) kons?)
  (is 42 kons?)]

Построим конструктор для списков, создаваемых с помощью @racket[kons],
при этом, в качестве пустого списка, будем использовать символ @racket['knull]:

@def+int[#:eval formica-eval
 (define (klist . x)
   (foldr kons 'knull x))
 (klist 1 2 3 4)]

Так можно определить контракт @racket[klist?], определяющий тип для подобных списков:
@def+int[#:eval formica-eval
   (define-type klist? 
     'knull
     (kons: Any klist?))
   (is (klist 1 2 3) klist?)
   (is (klist) klist?)
   (is (kons 1 (kons 2 3)) klist?)
   (is (list 1 2 3) klist?)]

Обратите внимание на то, что определение этого типа
соответствует формальному определению алгебраического типа для списков:
@racketgrammar[#:literals (null сons:) List null (cons: Any List)]

Наконец, определим свёртку и ряд функций для обработки 
определённых нами списков:

@def+int[#:eval formica-eval
  (define/c (kfold f x0)
    (/. 'knull --> x0
        (kons h t) --> (f h (kfold f x0 t))
        x --> (error "kfold: The argument must be a klist. Given" x)))]

Отображение:

@def+int[#:eval formica-eval
  (define/c (kmap f) (kfold (∘ kons f) 'knull))
  (kmap sqr (klist 1 2 3 4 5))
  (kmap sqr (list 1 2 3 4 5))]

Сумма элементов:

@def+int[#:eval formica-eval
  (define total (kfold + 0))
  (total (klist 1 2 3 4 5))]

@bold{Пример 2.}

Определим параметризованный алгебраический тип для представления двоичного дерева 
с элементами заданного типа @racket[A].
@racketgrammar[#:literals (Empty) (Tree A) Empty (Leaf A) (Node (Tree A) (Tree A))]

@defs+int[#:eval formica-eval
  ((define-type (Leaf Any))
  (define-type (Node Any Any))
  (define-type (Tree A) 
    'Empty
    (Leaf: A)
    (Node: (Tree A) (Tree A))))]

Теперь мы можем создавать двоичные деревья

@defs+int[#:eval formica-eval
  [(define A (Node (Node (Leaf 5) 
                        'Empty) 
                  (Node (Node 'Empty 
                              (Leaf 6)) 
                        (Leaf 2))))
  (define B (Node (Leaf 'a) 
                         (Node (Leaf 'b) 
                               (Node (Leaf 'c)
                                     'Empty))))]
  ((Tree integer?) A)
  ((Tree Sym) A)
  ((Tree Sym) B)]

Определим для нашего типа функцию свёртки:

@def+int[#:eval formica-eval
  (define/c (tfold f x0 g)
    (/. 'Empty --> x0
        (Leaf x) --> (g x)
        (Node l r) --> (f (tfold f x0 g l)
                          (tfold f x0 g r))
        x --> (error "The argument must be a Tree. Given" x)))]

С её помощью можно определить ряд полезных функций для обработки деревьев:

Отображение:

@def+int[#:eval formica-eval
   (define/c (tmap f) (tfold Node 'Empty (∘ Leaf f)))
   (tmap sqr A)]

Список листьев:

@def+int[#:eval formica-eval
   (define leaves (tfold append '() list))
   (leaves A)
   (leaves B)
   (leaves '(1 2 3))]

Сумма элементов (для числовых деревьев):

@def+int[#:eval formica-eval
   (define (total t)
     (cond
       [(is t (Tree Num)) (tfold + 0 id t)]
       [else (error "total: The argument must be a (Tree Num). Given" t)]))
   (total A)
   (total B)]

Для проверки типов можно определить @elemref["t:signature"]{сигнатуру} функции:
@def+int[#:eval formica-eval
   (:: total ((Tree Num) -> Num)
     (define total (tfold + 0 id)))
   (total A)
   (total B)]

@include-section["contracts.scrbl"]