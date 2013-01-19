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

@title[#:tag "pairs"]{Функции для работы с составными данными}

@section[#:tag-prefix "pairs"]{Функции для работы с парами}

@elemtag["t:pair"]{@emph{Точечная пара}} --- абстрактный тип данных с конструктором @racket[cons] и
селекторами @racket[car] и @racket[cdr], удовлетворяющими уравнениям:

@centered[@racket[(car (cons _x _y)) = _x]]
@centered[@racket[(cdr (cons _x _y)) = _y]]

@centered[@racket[(cons (car _p) (cdr _p)) = _p]]


@elemtag["t:liststr"]{@emph{Списочной структурой}} называется последовательность вложенных точечных пар:

@centered[@racket[(cons _a (cons _b ... (cons _x _y)))]]

Элементами списочной структуры могут быть любые объекты, в том числе и другие списочные структуры.

@defproc[(cons [a Any] [b Any]) pair?]
Конструктор точечной пары. Используется для создания пар и списочных структур.

Примеры:
@interaction[#:eval formica-eval
 (cons 1 2)
 (cons 'a (cons 'b 'c))
 (cons (cons 1 'a) (cons 2 'b))]
  

@defproc[(car [p pair?]) any]
Селектор первого элемента пары. 

Примеры:
@interaction[#:eval formica-eval
 (car (cons 1 2))
 (car (cons 'a (cons 'b 'c)))
 (car (cons (cons 1 'a) (cons 2 'b)))
 (car (car (cons (cons 1 'a) (cons 2 'b))))]

@defproc[(cdr [p pair?]) any]
Селектор второго элемента пары. 

Примеры:
@interaction[#:eval formica-eval
 (cdr (cons 1 2))
 (cdr (cons 'a (cons 'b 'c)))
 (cdr (cons (cons 1 'a) (cons 2 'b)))
 (cdr (cdr (cons (cons 1 'a) (cons 2 'b))))]

@defproc[(pair? [x Any]) Bool]
Возвращает @racket[#t], если @racket[_x] является парой и @racket[#f] --- в любом другом случае.

Примеры:
@interaction[#:eval formica-eval
 (pair? (cons 1 2))
 (pair? 1)
 (pair? (cons 'a (cons 'b 'c)))
 (pair? (cdr (cons (cons 1 'a) (cons 2 'b))))]

@defthing[null null?]
Нулевая ссылка. 

Может быть использована, для построения бинарных деревьев

Примеры:
@interaction[#:eval formica-eval
 (cons (cons 1 null) (cons 2 (cons null 3)))]

@defproc[(null? [x Any]) Bool]
Возвращает @racket[#t], если @racket[_x] является нулевой ссылкой и @racket[#f] --- в любом другом случае.

Примеры:
@interaction[#:eval formica-eval
 (null? null)
 (null? 1)
 (null? (cdr (cons 'a null)))]

@section[#:tag-prefix "pairs"]{Функции для работы со списками}

@elemtag["t:list"]{@emph{Списками}} называются списочные структуры, 
имеющие самым правым элементом нулевую ссылку @racket[null].

@centered[@racket[(cons _a (cons _b (cons _c null))) = (list _a _b _c)]]

@defproc[(list [x Any] ...) list?]
Конструктор списка.

Примеры:
@interaction[#:eval formica-eval
 (list 1 2 3)
 (cons 1 (cons 2 (cons 3 null)))]

@defproc[(list? [x Any]) Bool]
Возвращает @racket[#t], если @racket[_x] является списком и @racket[#f] --- в любом другом случае.

Примеры:
@interaction[#:eval formica-eval
 (list? (list 1 2 3))
 (list? 1)
 (list? (cons 1 (cons 2 (cons 3 null))))
 (list? (list))
 (list? empty)
 (list? (cons 1 2))]

@defthing[empty null?]
Пустой список. Эквивалентен @racket[null].

Примеры:
@interaction[#:eval formica-eval
 empty
 (list)
 (cons 'a empty)
 (eq? empty '())
 (eq? null empty)]

@defproc[(empty? [x Any]) Bool]
Возвращает @racket[#t], если @racket[_x] является пустым списком и @racket[#f] --- в любом другом случае.

Примеры:
@interaction[#:eval formica-eval
 (empty? empty)
 (empty? (cons 1 2))
 (empty? 1)
 (empty? (list 1 2 3))]

Приведём некоторые часто используемые функции для создания и обработки списков:


@defproc*[[[(range [n Real]) (listof Real)]
           [(range [n Real] [m Real]) (list: Real ..)]
           [(range [n Real] [m Real] [step Real]) (list: Real ..)]]]
Создаёт список чисел от @racket[_n] до @racket[_m] с шагом @racket[_step]. 
Если шаг не указан, используется шаг @racket[1]. 
Если указан только один предел, создаётся список от @racket[1] до этого предела.

Примеры:
@interaction[#:eval formica-eval
 (range 5)
 (range 2 5)
 (range 2 15 3)
 (range -2/3 4 1/3)
 (range 2.1 5 1.2)]

@defproc[(map [f Fun] [lst list?] ...) list?]
Функция отображения списков @racket[_lst ...] функцией @racket[_f].

Примеры:
@interaction[#:eval formica-eval
 (define-formal f)
 (map f '(a b c))
 (map f '(a b c) '(x y z))]
 
@deftogether[
[@defproc[(foldr [f Fun] [x0 Any] [lst list?] ...) Any]
  @defproc[(foldl [f Fun] [x0 Any] [lst list?] ...) Any]]]
Функции правой (@racket[foldr]) и левой (@racket[foldr]) свёртки 
списка @racket[_lst] функцией @racket[_f].

Примеры:
@interaction[#:eval formica-eval
 (define-formal f)
 (foldr f 'x0 '(a b c))
 (foldl f 'x0 '(a b c))
 (foldr f 'x0 '(a b c) '(x y z))
 (foldl f 'x0 '(a b c) '(x y z))]

@defproc[(filter [f (Any → Bool)] [lst list?]) list?]
Функция фильтрации списка @racket[_lst] с помощью
функции @racket[_f].

Примеры:
@interaction[#:eval formica-eval
 (filter odd? '(1 2 1 5 7 2 6 3 8))
 (filter (< 5) '(1 2 1 5 7 2 6 3 8))]

@defproc[(member [v Any] [lst list?]) Bool]
Если хотя бы один из элементов списка @racket[_lst] эквивалентен @racket[_v], возвращает элементы списка, начиная с первого вхождения элемента @racket[_v] и @racket[#f] -- если  @racket[_v] в список не входит. Для сравнения используется отношение @racket[equal?].

Примеры:
@interaction[#:eval formica-eval
 (member 'b '(a b c))
 (member 'd '(a b c))
 (member 3. '(1 2 3))]

О дополнительных функциях для работы со списками можно узнать из документации к модулям
@racket[racket/base] и @racket[racket/list].