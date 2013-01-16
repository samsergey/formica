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

@title[#:tag "functional"]{Операторы и функционалы}

@defform/subs[#:id λ (λ vars body)
([vars (x ...)
       (x ... . y)
       x])]
Создаёт анонимную функцию с телом @racket[_body] и формальными аргументами @racket[_vars]:
@itemize{
 @item{@racket[(x ...)] --- фиксированная последовательность аргументов.}
 @item{@racket[(x ... . y)] --- последовательность аргументов, из которых фиксированные перечисляются как @racket[(x ...)] 
        а символ @racket[y] связывается со списком произвольного числа фактических аргументов (для вариадических функций).}
 @item{@racket[x] --- последовательность аргументов (для вариадических функций).  
       При этом символ @racket[x] связывается со списком фактических аргументов.}}

Примеры:
@interaction[#:eval formica-eval
  (λ (x) (* 2 x))
  ((λ (x) (* 2 x)) 4)
  (map (λ (x) (* 2 x)) '(1 2 3))
  ((λ (x y) (+ y (* 2 x))) 4 5)
  ((λ (x y . z) (list x y z)) 1 2 3 4)
  ((λ (x y . z) (list x y z)) 1 2 3)
  ((λ (x y . z) (list x y z)) 1 2)]


@defproc*[[[(apply [f Fun] [arg-list list?]) Any]
           [(apply [f Fun] [arg Any] ... [arg-list list?]) Any]]]
Применяет функцию @racket[_f] к списку аргументов @racket[_arg-list].
Если помимо списка аргументов, заданы аргументы @racket[_arg ...], 
они присоединяются к списку @racket[_arg-list].

Примеры:
@interaction[#:eval formica-eval
  (apply + '(1 2 3))
  (apply (λ (x) (* 2 x)) '(1))
  (apply map + '((1 2 3) (10 20 30)))]

@margin-note{Определена в библиотеке @racket[formica/functionals]}
@defproc[(function? [x Any]) Bool]
Возвращает @racket[#t] только если @racket[_x] является функцией 
и @racket[#f] --- во всех других случаях.


@margin-note{Определена в библиотеке @racket[formica/functionals]}
@defthing[id Fun]
Тождественная функция.

Примеры:
@interaction[#:eval formica-eval
  (id 1)
  (id 'x)
  (id +)
  ((id +) 1 2)
  (id id)
  (id '(a b c))]


@margin-note{Определена в библиотеке @racket[formica/functionals]}
@defproc[(arg (n Index)) Fun]
Создаёт тривиальную функцию, возвращающую @racket[_n]-ый 
переданный ей аргумент.

Примеры:
@interaction[#:eval formica-eval
  ((arg 1) 'x 'y 'z)
  ((arg 2) 'x 'y 'z)
  ((arg 3) 'x 'y 'z)]

@defthing*[([I1 (arg 1)]
            [I2 (arg 2)]
            [I3 (arg 3)])]

Псевдонимы для часто встречающихся вызовов функции @racket[arg].

@margin-note{Определена в библиотеке @racket[formica/functionals]}
@defproc[(const (x Any)) Fun]
Создаёт тривиальную функцию, возвращающую @racket[_x] для 
любых переданных ей аргументов.

Примеры:
@interaction[#:eval formica-eval
 (const 'A)
 ((const 'A) 1)
 ((const 'A) 1 2)
 ((const +))]

@deftogether[[@defproc[(composition [f Fun] ...) Fun]
@defthing[#:kind "alias" ∘ composition]]]
Возвращает ообщённую композицию функций @racket[_f ...], которые могут иметь произвольную валентность: композиция вычисляется так, как если бы они все были каррированы.

При обобщённой композиции вариадическе функции имеют минимально возможную валентность, если они не являются @elemref["greedy"]{"жадными"}.

Функция @racket[composition] иеет псевдоним: @racket[∘] (может быть введён как @litchar{\circ} + Alt @litchar{\}).

Примеры:
@interaction[#:eval formica-eval
  (define-formal (u 1) (b 2) (t 3))
  ((∘ u b) 1 2)
  ((∘ b u) 1 2)
  ((∘ t b u) 1 2 3 4)
  ((∘ t u b) 1 2 3 4)
  ((∘ u t b) 1 2 3 4)
  ((∘ u b t) 1 2 3 4)
  ((∘ b u t) 1 2 3 4)
  ((∘ b t u) 1 2 3 4)
  ((∘ length (filter odd?)) '(1 2 3 4))]


Композиция ассоциативна и имеет правый и левый нейтральный элемент:
@interaction[#:eval formica-eval
  ((∘ (∘ b t) u) 1 2 3 4)
  ((∘ b (∘ t u)) 1 2 3 4)]

@interaction[#:eval formica-eval
  ((∘ b id) 1 2)
  ((∘ id b) 1 2)]


При обобщённой композиции вариадическе функции имеют минимально возможную валентность:
@interaction[#:eval formica-eval
  (define (f . x) (cons 'f x))
  (define (g x . y) (list* 'g x y))
  ((∘ f g) 1 2 3 4)]

@defproc[(greedy [f Fun]) Fun]
Возвращает @elemtag["greedy"]{"жадную"} вариадическую функцию, которая функционально эквивалентна @racket[_f], но имеет максимально возможную валентность.

Примеры:
@interaction[#:eval formica-eval
  (define-formal f g h)
  ((∘ f g h) 1 2 3 4)
  ((∘ f (greedy g) h) 1 2 3 4)
  ((∘ remove-duplicates append) '(1 2 3) '(2 3 2 4))
  ((∘ remove-duplicates (greedy append)) '(1 2 3) '(2 3 2 4))]


 
@margin-note{Определена в библиотеке @racket[formica/functionals]}
@deftogether[[
 @defproc[(negated (p Fun)) Fun]
  @defthing[#:kind "alias" ¬ negated]]]
Возвращает отрицание предиката @racket[_p].

Функция @racket[negated] иеет псевдоним: @racket[¬] (может быть введён как @litchar{\neg} + Alt @litchar{\}).

Примеры:
@interaction[#:eval formica-eval
  (negated odd?)
  ((negated odd?) 2)
  (define ≥ (¬ <))
  (map ≥ '(1 2 3) '(3 2 1))]

@margin-note{Определена в библиотеке @racket[formica/functionals]}
@defproc[(flipped [f Fun]) Fun]
Возвращает функцию, функционально-эквивалентную @racket[_f], но принимающую 
аргументы в обратном порядке.

Примеры:
@interaction[#:eval formica-eval
  (define snoc (flipped cons))
  snoc
  (snoc 1 2)
  ((flipped list) 1 2 3)]

@margin-note{Определена в библиотеке @racket[formica/functionals]}
@defproc[(fif [p Fun] [f Fun] [g Fun]) Fun]
Возвращает функцию @tt{x y ... -> (if (p x y ...) (f x y ...) (g x y ...))}.

Примеры:
@interaction[#:eval formica-eval
  (map (fif odd? sub1 id) '(1 2 3 4))]

@margin-note{Определена в библиотеке @racket[formica/functionals]}
@defproc[(andf [f Fun] [g Fun] ...) Fun]
Возвращает функцию @tt{x y ... -> (and (f x y ...) (g x y ...) ...)}.

Примеры:
@interaction[#:eval formica-eval
  (map (andf integer? positive?) '(-3/5 -1 0 2 4.2))]

@margin-note{Определена в библиотеке @racket[formica/functionals]}
@defproc[(orf [f Fun] [g Fun] ...) Fun]
Возвращает функцию @tt{x y ... -> (or (f x y ...) (g x y ...) ...)}.

Примеры:
@interaction[#:eval formica-eval
  (map (orf integer? positive?) '(-3/5 -1 2 4.2))]

@margin-note{Определена в библиотеке @racket[formica/functionals]}
@deftogether[[
 @defproc[(fork [f Fun] [g unary?]) Fun]
  @defthing[#:kind "alias" -< fork]]]
Возвращает функцию @tt{x y ... -> (f (g x) (g y) ...)}.

Функция @racket[fork] иеет псевдоним: @racket[-<]

Примеры:
@interaction[#:eval formica-eval
  ((fork cons sqr) 2 3)
  ((-< + magnitude) -2 3 4-7i)]

@margin-note{Определена в библиотеке @racket[formica/functionals]}
@defproc[(fixed-point [f Fun]) Fun]
Возвращает функцию, отыскивающую неподвижную точку функции @racket[_f] с помощью 
итеративной аппликации. Для любого аргумента @racket[x], она
вычисляет @racket[(_f (_f (_f ... (_f x))))] до тех пор, пока результат
не перестанет изменяться.

Пример:
@interaction[#:eval formica-eval
  ((fixed-point cos) 1)
  (cos ((fixed-point cos) 1))]

@margin-note{Определена в библиотеке @racket[formica/curry]}
@defproc[(curry [f Fun] [arg Any] ...) (or/c curried? Any)]
Каррирует функцию @racket[_f], фиксируя аргументы @racket[_arg ...] слева.

Если аргументы @racket[_arg ...] не заданы возвращается каррированная функция @racket[_f].

Примеры:
@interaction[#:eval formica-eval
  (curry list 1 2)
  ((curry list 1 2) 3 4)
  (map (curry cons 1) '(1 2 3))
  (curry +)
  ((curry +) 1)
  ((curry +) 1 2)
  (curry cons 1 2)]

@margin-note{Переопределена в библиотеке @racket[formica/functionals]}
@defproc[(curryr [f Fun] [arg Any] ...) (or/c curried? Any)]
Каррирует функцию @racket[_f], фиксируя аргументы @racket[_arg ...] справа.

Примеры:
@interaction[#:eval formica-eval
  (curryr list 1 2)
  ((curryr list 1 2) 3 4)
  (map (curryr cons 1) '(1 2 3))
  (curryr list)
  ((curryr list) 1)
  ((curryr list) 1 2)
  (curryr cons 1 2)]

@margin-note{Определена в библиотеке @racket[formica/functionals]}
@defproc[(curried? [f Any]) Bool]
Возвращает @racket[#t], если @racket[_f] является каррированной функцией 
и @racket[#f] --- в любом другом случае.

Примеры:
@interaction[#:eval formica-eval
  (curried? (curry + 1))
  (curried? cons)
  (curried? (curry cons))
  (curried? (curry cons 1))
  (curried? ((curry cons) 1 2))]