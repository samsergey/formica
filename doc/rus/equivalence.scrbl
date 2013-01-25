#lang scribble/doc

@(require (for-label formica))

@(require
  scribble/manual
  scribble/eval)

@(define formica-eval
   (let ([sandbox (make-base-eval)])
     (sandbox '(require formica))
     sandbox))

@title[#:tag "equiv"]{Отношения эквивалентности и порядка}

@declare-exporting[formica]

@section{Отношение эквивалентности}

@defproc[(eq? [v1 Any] [v2 Any] ...) Bool]
Возвращает @racket[#t], если @racket[_v1] и @racket[_v2] связаны с одним и тем же объектом.

@interaction[#:eval formica-eval
 (eq? 'a 'a) 
 (eq? 'a 'b)
 (define b 'a)
 (eq? b 'a)
 (eq? 1 1)
 (eq? 1 1.0)
 (eq? "abc" "abc")
 (eq? (cons 1 2) (cons 1 2))
 (eq? 1 1 1)]


@defproc[(equal? [v1 Any] [v2 Any] ...) Bool]
Возвращает @racket[#t], если @racket[_v1] и @racket[_v2] имеют одинаковую структуру и их элементы эквивалентны в смысле @racket[eq?].

@interaction[#:eval formica-eval
 (equal? 'a 'a) 
 (equal? 'a 'b)
 (define b 'a)
 (equal? b 'a)
 (equal? 1 1)
 (equal? 1 1.0)
 (equal? "abc" "abc")
 (equal? (cons 1 2) (cons 1 2))
 (equal? 1 1.0 1+0i)]

@defproc[(different? [v1 Any] [v2 Any] ...+) Bool]
Возвращает @racket[#t], если @racket[_v1] и @racket[_v2] не эквивалентны в смысле @racket[equal?], и @racket[#f] в противном случае. Если задано более двух аргументов, они сравниваются попарно, и @racket[#t] возвращается лишь в том случае, если все аргументы различны.

Примеры:
@interaction[#:eval formica-eval
  (different? 1 1)
  (different? 1 2)
  (different? 1 1.0)
  (different? 1 2 6 3 9 7)
  (different? 1 2 6 3 2 7)]

@defproc[(almost-equal? [v1 Any] [v2 Any] ...+) Bool]
@defthing[≈ almost-equal?]
Возвращает @racket[#t], если @racket[_v1] и @racket[_v2] эквивалентны в смысле @racket[equal?], или, для числовых значений, если они равны с относительной погрешностью @racket[(tolerance)]. В противном случае возвращает @racket[#f].

Функция @racket[almost-equal?] имеет синоним: @racket[≈] (может быть введён, ка @litchar{\approx} + @onscreen{Alt} @litchar{\}).

Примеры:
@interaction[#:eval formica-eval
  (≈ 'x 'x)
  (≈ 1 'x)
  (≈ 1 1.)
  (≈ 1/2 (+ 0.5 1e-15))
  (≈ 1/2 (+ 0.5 1e-16))
  (≈ 1/2 (+ 0.5 0+1e-16i))
  (≈ 0.5 1/2 (+ 0.5 0+1e-16i))
  (≈ '(1 (2 3)) '(1 (2.000000000000001 3)))]

@defparam[tolerance x Real]
Параметр, определяющий относительную погрешность для предиката @racket[almost-equal?] predicate. По умолчанию, равен @racket[5e-16].

Примеры:
@interaction[#:eval formica-eval
  (parameterize ([tolerance 0.01])
    (and (≈ 1 1.001)
         (≈ 10 10.01)
         (≈ 1e23 1.001e23)
         (≈ 1e-23 1.001e-23)
         (≈ 0 0.001)))]

В последнем случае использование относительной погрешности невозможно, и значение @racket[(tolerance)] интерпретируется как абсолютная погрешность.

@section{Отношение порядка}

Символьные проебразования часто требуют упорядочивания разнородных объёктов: чисел, символов, строк, списков и т.д. Например, следующие два алгебраических выражения должны быть эквивалентны @centered{@tt{'(+ a (* b с)) = '(+ (* с b) a).}}
Таким образом, имеет смысл упорядочивать аргументы в символьном представлении коммутативных операций.

Formica предоставляет возможность определять отношение порядка над произвольными множествами, которые представляются в виде типов.

В библиотеке @racket[formica/types] определется следующее отношение порядка:
@tabular[#:sep @hspace[2]
         (list (list @bold{Порядок} @bold{Тип (множество)} @bold{Отношение порядка})
               (list "1"   @racket[#t]    @racket[(const #f)])
               (list "2"   @racket[#f]    @racket[(const #f)])
               (list "3"   @racket[Real]    @racket[<])
               (list "4"   @racket[Str]    @racket[string<?])
               (list "5"   @racket[Sym]    @racket[symbol<?])
               (list "6"   @racket[null?]    @racket[(const #f)])
               (list "7"   @racket[pair?]    @racket[pair<?]))]
Это значит, например, что любой элемент типа строк следуют за любым числом, а для упорядочивания элементов внутри множеств используются указанные предикаты, задающие отношение порядка: @racket[string<?] -- для строк и @racket[<] -- для действительных чисел.
Эта таблица представлена в виде ассоциативного массива в параметре @racket[(type-ordering)] и может быть изменена или дополнена.

@defproc[(ordered? [v Any] ...) Bool]
Возвращает @racket[#t], если аргументы @racket[_v ...] упорядочены в соответствии с отношением @racket[(type-ordering)], в противном случае, возвращает @racket[#f]. Элементы множеств, не имеющих отношения порядка всегда следуют за элементами упорядоченных множеств.

Примеры:
@interaction[#:eval formica-eval
 (ordered? 1)
 (ordered? 'x 'y)
 (ordered? 1 "a" 'x)
 (ordered? 1 "a" 'x #f)
 (sort '(#f 'y (3 2) 2.0 "ab" 'x 'b 3 "abc" 'a "bca" (1 (2 3)) (1 2) 1 (1) #t) ordered?)]

@defparam[type-ordering p (list: (cons: Type (Any Any → Bool)))]
Параметр, задающий отношение порядка между элементами различных типов и элементами внутри типов.

В следующем примере чётные числа следуют за нечётными, причём нечётные числа следуют в обратном порядке. Символы не включены в таблицу @racket[(type-ordering)] и остаются неупорядоченными, но следуют за числами.
@interaction[#:eval formica-eval
 (parameterize ([type-ordering (list (cons odd? >)
                                     (cons even? <))])
   (sort '(0 1 2 3 'x 4 5 6 'a 7 8 9) ordered?))]

@defproc[(add-to-type-ordering (type Type) (prec-type (or/c Type 'last 'first) 'last) (ord-fun (Any Any → Bool) (cons #f))) void?]
Дополняет текущую таблицу отношений порядка @racket[(type-ordering)] типом @racket[_type]. Если задан тип @racket[_prec-type], добавляемый тип будет следовать непосредственно за ним. Если задан бинарный предикат @racket[_ord-fun], то он будет использован для упорядочивания элементов типа @racket[_type]. 

В этом примере комплексные числа следуют за действительными, а внутри множества комплексных чисел для упорядочивания сравниваются их модули. Символы упорядочиваются обычным образом.
@interaction[#:eval formica-eval
 (parameterize ([type-ordering (type-ordering)])
   (add-to-type-ordering complex? Real (/@ < magnitude))
   (sort '(0-i 2 3.5 1+2i "x" 4-i 5 6 "a" 7 -1+2.5i 9) ordered?))]

@defproc[(symbol<? [s1 Sym] [s2 Sym]) Bool]
Определяет лексикографический порядок на множестве символов.

Примеры:
@interaction[#:eval formica-eval
  (symbol<? 'x 'y)
  (symbol<? 'abcde 'acde)
  (symbol<? 'x 'x)
  (sort '(a X abc x abcd A) symbol<?)]

@defproc[(pair<? [p1 pair?] [p2 pair?]) Bool]
Определяет лексикографический порядок на множестве пар элементов упорядоченных множеств.

Примеры:
@interaction[#:eval formica-eval
  (pair<? '(1 2) '(1 2 3))
  (pair<? '(1 2) '(1 3))
  (pair<? '(1 . 2) '(1 2))
  (pair<? '(1 2) '(1 x y))]