#lang scribble/doc

@(require (for-label formica))

@(require
  scribble/manual
  scribble/eval)

@(define formica-eval
   (let ([sandbox (make-base-eval)])
     (sandbox '(require formica))
     sandbox))

@title[#:tag "monad:defined"]{Монады, определённые в Formica}

@declare-exporting[formica]

@section[#:tag "monad:Id"]{Тождественная монада}

@defthing[Id monad?] 
Тождественная монада.

Определение:
@codeblock{return = id
           bind _m _f = (_f _m)}

Примеры:
@defs+int[#:eval formica-eval
 ((using-monad Id)
 (define-formal f))
 (return 'x)
 (bind 'x >>= f)]

@interaction[#:eval formica-eval
 (do [x <- 5]
     [y <- 8]
     (displayln x)
     [x <- (+ x y)]
     (list x y))]

@interaction[#:eval formica-eval
 (do [(cons x y) <- '(1 2 3)]
     [(z t) <<- (reverse y)]
     (return (list x y z t)))]

@section[#:tag "monad:seq"]{Неоднозначные вычисления.}

Описанные в этом разделе монады, предназначены для работы с многозначными или частично-определёнными функциями (функциями, неопределёнными для некоторых значений).
Под последовательностью в этом разделе понимаются любые индуктивные множества: натуральные числа, списки, строки, потоки и т.п.

@defproc[(Monoid [#:return ret (Any ... → listable?)]
                   [#:mplus seq-append (listable? listable? → listable?)]
                   [#:map seq-append-map ((Any → listable?) listable? → listable?) mplus-map]) monad-plus?]
Возвращает монаду, предназначеннную для работы с вычислениями, которые могут возвращать последовательность значений. Это обобщение монад @racket[List], @racket[Stream] и @racket[Amb].

Определение:
@codeblock{return = _ret
           bind _s _f = (_seq-append-map _f _s)
           mplus = _seq-append
           mzero = (return)
           type = listable?
           failure = (const mzero)}

Последовательность возможных аргументов конструируется с помощью функции  @racket[_ret]. При связывании, функция @racket[_f] применяется ко всем возможным значениям аргументов из последовательности @racket[_s] и все возможные результаты объединяются с помощью операции @racket[_seq-append], производя последовательность всех возможных результатов. Особенности процесса связывания могут быть заданы функцией  @racket[_seq-append-map].

Функция @racket[Monoid] создаёт монады, оперирующие с моноидами: множествами с определёнными на них нейтральным элементом (нулём) @racket[(_ret)] и бинарной операцией соединения двух значений в одно (сложением) @racket[_seq-append].

@bold{Примеры:}

Используя @racket[Monoid] легко определить монады для различных типов последовательностей: множеств, векторов, строк и т.п.
@def+int[#:eval formica-eval
 (define-monad Set
   (Monoid
    #:return set
    #:mplus set-union))
 (using Set
   (lift/m + '(1 2 3) '(2 3 4)))]

@defs+int[#:eval formica-eval
 ((define-monad String
   (Monoid
    #:return string
    #:mplus (flipped string-append))))
 (using String
  (collect (char-upcase x)
    [x <- "abc12xy"] 
    (char-alphabetic? x)
    [x <- (format "~a(~a)" x (char->integer x))]))]

@defproc[(listable? (v Any)) Bool]
Возвращает @racket[#t] если @racket[v] является последовательностью, но не является формальной аппликацией.

Примеры:
@interaction[#:eval formica-eval
  (listable? '(1 2 3))
  (listable? 4)
  (listable? (stream 1 2 (/ 0)))
  (listable? '(g x y))
  (define-formal g)
  (listable? (g 'x 'y))]

@defproc[(mplus-map [f (Any → listable?)] [s listable?]) listable?]
Обобщённая функция отображения в аддитивной монаде. Формально определена, как 
@codeblock{(mplus-map f s) = (foldl (∘ mplus f) mzero s)}

@defproc[(zip [s listable?] ...) sequence?]
Возвращает последовательность, в которой каждый элемент является списком из последовательно выбираемых элементов последовательностей @racket[_s ...]. Используется для параллельной обработки последовательностей.

Пример:
@interaction[#:eval formica-eval
  (using List
    (do [(list x y) <- (zip '(a b c) 
                            '(1 2 3 4))]
      (return (f x y))))]

@subsection[#:tag "monad:List"]{Монада List}

@defthing[List monad-plus?] 
Монада @racket[List] используется для генерации списков и производства жадных вычислений с многозначными или частично-определёнными функциями.

Определение:
@codeblock{List = (Monoid #:return list
                            #:mplus concatenate
                            #:map concat-map)}

@defproc[(concatenate (s listable?) ...) list?]
Объединяет последовательности @racket[_s ...] в список.

Примеры:
@interaction[#:eval formica-eval
  (concatenate '(1 2 3) '(a b c))
  (concatenate 4 (stream 'a 'b 'c))
  (concatenate (in-set (set 'x 'y 'z)) (in-value 8))
  (concatenate 1 2 3)]

@defproc[(concat-map (f (Any → n/f-list?)) (s listable?)) list?]
Применяет функцию @racket[_f] к элементам последовательности @racket[_s] и возвращает объединение результатов в виде списка.

Примеры:
@interaction[#:eval formica-eval
  (concat-map (λ (x) (list x (- x))) '(1 2 3))
  (concat-map (λ (x) (list x (- x))) 4)]

@bold{Примеры использования монады List}

@def+int[#:eval formica-eval
 (using-monad List)]

Примеры генерации списков
@interaction[#:eval formica-eval
 (collect (sqr x) [x <- '(1 2 5 13 4 24)] (odd? x))]

@interaction[#:eval formica-eval
 (collect (cons x y) 
   [x <- '(1 3 4 8 2 4)]
   [y <- '(3 1 6 4 3)]
   (< x y)
   (= (modulo x y) 2))]

@interaction[#:eval formica-eval
 (collect (cons x y) 
   [(list x y) <- '((1 2) (2 4) (3 6) (5 1))]
   (odd? x)
   (< x y))]

@interaction[#:eval formica-eval
 (collect (cons x y) [(x y) <<- 10] (< 4 (+ (sqr x) (sqr y)) 9))]

@interaction[#:eval formica-eval
 (lift/m cons 2 "abc")]

Использование параллельной обработки последовательностей:
@interaction[#:eval formica-eval
  (using List
    (do [a <- '(x y z)]
        [(list x y) <- (zip "AB" 
                            (in-naturals))]
      (return (f a x y))))]

@subsection[#:tag "monad:Stream"]{Монада Stream}

@defthing[Stream monad-plus?] 
Подобна монаде @racket[List], но производит вычисления лениво и результат возвращается в виде потока.

Определение:
@codeblock{Stream = (Monoid #:return list
                              #:mplus stream-concatenate
                              #:map stream-concat-map)}

@defproc[(stream-concatenate (s listable?) ...) stream?]
Возвращает результат ленивого объединения последовательностей @racket[_s ...] в виде потока.

Примеры:
@interaction[#:eval formica-eval
  (stream->list 
   (stream-concatenate '(1 2 3) '(a b c)))
  (stream->list 
   (stream-concatenate 4 (stream 'a 'b 'c)))
  (stream-ref 
   (stream-concatenate (stream 1 (/ 0)) (in-naturals)) 
   0)
  (stream-ref 
   (stream-concatenate (stream 1 (/ 0)) (in-naturals)) 
   1)]

@defproc[(stream-concat-map (f (Any → stream?)) (s listable?)) stream?]
Лениво применяет функцию @racket[_f] к элементам последовательности @racket[_s] и возвращает объединение результатов.

Примеры:
@interaction[#:eval formica-eval
  (stream->list 
   (stream-concat-map (λ (x) (stream x (- x))) '(1 2 3)))
  (stream->list 
   (stream-concat-map (λ (x) (stream x (- x))) 4))
  (stream-ref 
   (stream-concat-map (λ (x) (stream x (/ x))) '(1 0 3)) 
   0)
  (stream-ref 
   (stream-concat-map (λ (x) (stream x (/ x))) '(1 0 3)) 
   1)
  (stream-ref 
   (stream-concat-map (λ (x) (stream x (/ x))) '(1 0 3)) 
   2)
  (stream-ref 
   (stream-concat-map (λ (x) (stream x (/ x))) '(1 0 3)) 
   3)]

@defproc[(stream-take (s stream?) (n Nat)) list?]
Возвращает список первых @racket[_n] элементов потока (или последовательности) @racket[s].

Примеры:
@interaction[#:eval formica-eval
  (stream-take (stream 'a 'b 'c) 2) 
  (stream-take (stream 'a 'b (/ 0)) 2)
  (stream-take (in-naturals) 3)]

@defform[(scons h t)]
Шаблон для непустого потока с первым элементом @racket[_h], и хвостом @racket[_t]. При сопоставлении вычисляется только первый элемент.

Примеры:
@interaction[#:eval formica-eval
 (require racket/match)
 (match (in-naturals)
   [(scons h t) (list h t)])
 (match (stream 1 (/ 0))
   [(scons h t) (list h t)])]

@bold{Примеры применения монады Stream}

@def+int[#:eval formica-eval
 (using-monad Stream)]

Два классических примера генерации бесконечных последовательностей

Последовательность пифагоровых треугольников:
@def+int[#:eval formica-eval
 (define triples 
  (collect (list a b c) 
    [a <- (in-naturals)]
    [b <- (in-range (ceiling (/ a 2)) a)]
    [c <-: (sqrt (- (sqr a) (sqr b)))]
    (integer? c)
    (> b c)))
(stream-take triples 3)
(stream-ref triples 100)]

Первый треугольник с площадью, превышающей 100: 
@interaction[#:eval formica-eval
  (stream-first 
   (collect t 
     [(and t (list _ b c)) <- triples] 
     (> (* 1/2 b c) 100)))]


Последовательность простых чисел:
@def+int[#:eval formica-eval
(define (primes r)
  (do [(scons x xs) <-: r]
      [p <-: (collect y 
               [y <- xs] 
               (not (zero? (modulo y x))))]
      (stream-cons x (primes p))))

(stream-take (primes (in-naturals 2)) 10)
(stream-ref (primes (in-naturals 2)) 100)]

В монаде @racket[Stream] все монадические функции работают лениво:
@interaction[#:eval formica-eval
 (stream-first ((compose/m (lift /) (lift (curry * 2))) 1/2 0))
 (stream-first (lift/m / (return 1) (return 1 0)))
 (stream-ref (lift/m / (return 1) (return 1 0)) 1)
 (stream-ref (map/m (λ (x) (stream x (/ x))) '(1 0 3)) 0)
 (stream-ref (map/m (λ (x) (stream x (/ x))) '(1 0 3)) 1)
 (stream-ref (map/m (λ (x) (stream x (/ x))) '(1 0 3)) 2)]

@subsection[#:tag "monad:Amb"]{Монада Amb}

@defthing[Amb monad-plus?] 
Подобна монаде @racket[Stream], но возвращает поток уникальных элементов.

Определение:
@codeblock{Amb = (Monoid #:return amb
                           #:mplus amb-union
                           #:map amb-union-map)}

@defproc[(amb (v Any) ...) stream?]
Возвращает поток уникальных агрументов @racket[_v ...].

Примеры:
@interaction[#:eval formica-eval
 (stream->list (amb 1 3 2 2 3 2 1 2 4 3))
 (stream-take (amb 1 2 (/ 0)) 2)]

@defproc[(amb-union (s1 listable?) (s2 listable?)) stream?]
Возвращает поток уникальных элементов последовательностей @racket[_s1] и @racket[_s2].

Примеры:
@interaction[#:eval formica-eval
  (stream->list 
   (amb-union '(1 2 3) '(2 3 4)))
  (stream->list 
   (amb-union 4 (amb 'a 'b 'c)))
  (stream-ref 
   (amb-union (amb 1 (/ 0)) (in-naturals)) 
   0)
  (stream-ref 
   (amb-union (amb 1 (/ 0)) (in-naturals)) 
   1)]

@defproc[(amb-union-map (f (Any → stream?)) (s listable?)) stream?]
Применяет функцию @racket[_f] к элементам последовательности @racket[_s] и возвращает поток уникальных результатов.

Примеры:
@interaction[#:eval formica-eval
  (stream->list 
   (amb-union-map (lift sqr) '(-3 -2 -1 0 -1 2 3)))
  (stream->list 
   (amb-union-map (λ (x) (amb x (- x))) 4))
  (stream-ref 
   (amb-union-map (λ (x) (amb x (/ x))) '(1 0 3)) 
   0)
  (stream-ref 
   (amb-union-map (λ (x) (amb x (/ x))) '(1 0 3)) 
   1)
  (stream-ref 
   (amb-union-map (λ (x) (amb x (/ x))) '(1 0 3)) 
   2)]

@bold{Примеры использования монады Amb}

@def+int[#:eval formica-eval
 (using-monad Amb)]

Доказательство логических утверждений методом полного перебора (построения таблицы истинности):
@interaction[#:eval formica-eval
 (stream->list 
  (collect (eq? (==> A B) (or B (not A))) 
    [(A B) <<- (amb #t #f)]))
 (stream->list 
  (collect (==> (==> (==> A B) C) (==> A (==> B C)))
    [(A B C) <<- (amb #t #f)]))
 (stream->list 
  (collect (eq? (==> (==> A B) C) (==> A (==> B C)))
    [(A B C) <<- (amb #t #f)]))
 (stream->list 
  (collect (list A B C)
    [(A B C) <<- (amb #t #f)]
    (not (eq? (==> (==> A B) C) (==> A (==> B C))))))]