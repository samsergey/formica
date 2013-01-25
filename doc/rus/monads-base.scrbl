#lang scribble/doc

@(require (for-label formica))

@(require
  scribble/manual
  scribble/eval)

@(define formica-eval
   (let ([sandbox (make-base-eval)])
     (sandbox '(require formica))
     sandbox))

@title[#:tag "monad:base"]{Базовые операции с монадами}

@declare-exporting[formica]

@section[#:tag "monad:constr"]{Конструирование монад}

Монады являются объектами первого класса. Следующая функция возвращает анонимную монаду, которую можно использовать в замыканиях для создания параметризованных монад.

@defproc[(monad [#:return return Fun]
                [#:bind bind (Any unary? → Any)]
                [#:mzero mzero Any 'undefined]
                [#:mplus mplus (Any Any → Any) 'undefined]
                [#:type type Type #f]
                [#:failure failure (Any → any) raise-match-error]) monad?]
Возвращает монаду (простую или аддитивную) с указанными функциями @racket[_return] и @racket[_bind], дополненными операцией @racket[_mplus] и нейтральным элементом @racket[_mzero] в случае аддитивной монады.

Ключи @racket[#:return] и @racket[#:bind] используются для наглядности и не могут быть опущены, однако их можно перечислять в произвольном порядке с прочими ключами.

Если указан тип @racket[_type], монадические величины должны иметь этот тип. Концепция монад очень хорошо сочетается с системой типов, обеспечивая согласованность последовательных вычислений. Указание типа монадических величин помогает отлаживать программы и делает их более надёжными.

Если указана функция @racket[_failure], она будет вызываться в случае невозможности сопоставления с образцом в формах @racket[do] и @racket[collect]. По умолчанию она выводит сообщение об ошибке.

@defform*[[(define-monad id m-expr)
           
           (define-monad id 
             #:return return 
             #:bind bind 
             [#:mzero mzero]
             [#:mplus mplus]
             [#:type type]
             [#:failure failure])]
         #:contracts ([m-expr monad?]
                      [return Fun]
                      [bind (Any unary? → Any)]
                      [mzero Any]
                      [mplus (Any Any → Any)]
                      [type Type]
                      [failure (Any → Any)])]
В первом варианте форма связывает именованную монаду с результатом, возвращаемым @racket[m-expr]. В втором варианте -- определяет именованную монаду с указанными свойствами так же, как функция @racket[monad].


@bold{Примеры:}

Простая монада-контейнер:
@defs+int[#:eval formica-eval
  ((define-formal (m 1) f g)
   
   (define-monad M
     #:return m
     #:bind (/. (m x) f --> (f x))))
  (using-monad M)]

Связывание в монаде:
@interaction[#:eval formica-eval
  (return 'a)
  (bind (m 'a) >>= (lift f))
  (bind (m 'a) >>= (lift f) >>= (lift g))
  (do [x <-: 'a]
      [y <-: (f x 'b)]
      (return (g y)))]

Монадические функции:
@interaction[#:eval formica-eval
  ((compose/m (lift f) (lift g)) 'a)
  (lift/m f (m 'a) (m 'b))]

Простая аддитивная монада (эквивалентная монаде @tt{Maybe}). В этом примере указывается тип монадических величин.
@defs+int[#:eval formica-eval
  ((define-formal (m 1) (z 0))
   
   (define-monad A/M
     #:type (or/c m? z?)
     #:return (/. (z) --> (z)
                   x  --> (m x))
     #:bind (/. (z)   f --> (z)
                (m x) f --> (f x))
     #:mzero (z)
     #:mplus (/. (z) x --> x
                  x  _ --> x)))
  (using-monad A/M)]

Примеры связывания:
@interaction[#:eval formica-eval
  (return 'a)
  (return (z))
  (bind (m 'a) >>= (lift f))
  (bind (z) >>= (lift f) >>= (lift g))
  (do [x <-: 'a]
      [y <-: (f x 'b)]
      (return (g y)))]
Примеры охраны:
@interaction[#:eval formica-eval
  (bind (m 2) >>= (guardf even?) >>= (lift g))
  (bind (m 2) >>= (guardf odd?) >>= (lift g))
  (do [x <-: 2]
      (guard (odd? x))
      [y <-: (f x 'b)]
      (return (g y)))
  (collect (g y)
    [x <-: 2]
    (odd? x)
    [y <-: (f x 'b)])]

Монадические функции:
@interaction[#:eval formica-eval
  ((compose/m (lift f) (lift g)) 'a)
  (lift/m f (m 'a) (m 'b))
  (lift/m f (m 'a) (z))
  (sum/m '((z) (z) (z) a b))]

Определение монады @racket[A/M] включает спецификацию типа. Это делает сообщения об ошибках более точными и понятными.
@interaction[#:eval formica-eval
  (bind 'a >>= (lift f))
  (bind (m 'a) >>= f)
  (lift/m f (m 'a) 'b)]


Параметризованная монада @tt{Maybe a}:
@defs+int[#:eval formica-eval
  ((define-formal Just)
   
   (define-type (Maybe? a) (Just: a) 'Nothing)
   
   (define (Maybe a)
     (monad
      #:type (Maybe? a)
      #:return (/. 'Nothing --> 'Nothing
                    x       --> (Just x))
      #:bind (/. 'Nothing f --> 'Nothing
                 (Just x) f --> (f x))
      #:mzero 'Nothing
      #:mplus (/. 'Nothing x --> x
                   x       _ --> x))))
  (using-monad (Maybe Int))
  (bind (Just 2) >>= (lift sqr) >>= (lift (* 2)))
  (bind 'Nothing >>= (lift sqr) >>= (lift (* 2)))  
  (bind (Just 4) >>= (lift sqrt))
  (bind (Just 2) >>= (guardf even?) >>= (lift (* 2)))
  (bind (Just 2) >>= (guardf odd?) >>= (lift (* 2)))
  (sum/m '(Nothing Nothing (Just 5)))]

@interaction[#:eval formica-eval
(using (Maybe Sym) (map mplus '(x y z) '(y z x)))]

Примеры с несоответствием типов:
@interaction[#:eval formica-eval
  (bind 4 >>= (lift sqrt))
  (bind (Just 2) >>= (lift sqrt))]

@defproc*[([(monad? [v Any]) Bool]
           [(monad-zero? [v Any]) Bool]
           [(monad-plus? [v Any]) Bool])]
Возвращают @racket[#t], если @racket[_v] является простой монадой, монадой с определённым нейтральным элементом или аддитивной монадой, соответственно.

Примеры:
@interaction[#:eval formica-eval
  (monad? Id)
  (monad-zero? Id)
  (monad-plus? Id)
  (monad? List)
  (monad-zero? List)
  (monad-plus? List)]

@section[#:tag "monad:using"]{Использование конктретной монады}

Все монады имеют одинаковый синтакс для связывания и одинаковые монадические функции, выбор конкретной монады определяет их семантику. В любой конкретный момент можно использовать лишь одну какую-нибудь монаду.

@defparam[using-monad m monad?]
Определеяет используемую монаду.

Примеры:
@interaction[#:eval formica-eval
 (using-monad)
 (using-monad Id)
 (using-monad)]


@defform[(using m expr ...) #:contracts ([m monad?])]
Вычисляет выражения @racket[_expr ...], используя монаду @racket[_m].

Примеры:
@interaction[#:eval formica-eval
 (using Id 
   (do [x <- 5]
       [x <- (+ x 6)]
       (return x)))
 (using List
   (do [x <- '(1 2 3)]
       [x <-: (+ x 6)]
       (return x)))]

@section[#:tag "monad:comp"]{Монадические вычисления}

@defform/subs[#:literals (>>= >>)(bind m <arr> f [ <arr> fs ...]) 
([<arr> >>= >>])]
Монадическое связывание. 

@itemize{
  @item{@racket[_m] @defidform/inline[>>=] @racket[_f] связывает функцию @racket[_f] и величину @racket[_m].}
  @item{@racket[_expr1] @defidform/inline[>>] @racket[_expr2] производит последовательное вычисление выражений @racket[_expr1] и @racket[_expr2]. Имеет смысл, если @racket[_expr1] производит побочный эффект.}}

Эта форма соответствует синтаксису языка @emph{Haskell} для монадического связывания:

@centered{@emph{Haskell:} @racket[m >>= f >> g ...]}

@centered{@emph{Formica:} @racket[(bind m >>= f >> g ...)]}

Примеры:
@interaction[#:eval formica-eval
 (using Id 
   (bind 5 >>= sqr))
 (using List
   (bind '(1 2 3) >>= (lift sqr)))]

@interaction[#:eval formica-eval
 (using Id 
   (bind 8 >>= displayln >> 5 >>= sqr))
 (using List
   (bind '(1 2 3) >>= (lift displayln) >> '(4 5 6) >>= (lift sqr)))]

Может использоваться, как функция:
@interaction[#:eval formica-eval
 (using List
   (apply bind (list '(a b c) (lift f))))]

@defform/subs[#:literals (<- <- <<- <<-:) (do ops ...+ res) 
([ops (pat <- expr) 
      (pat <-: expr) 
      ((pat1 pat2 ...) <<- expr) 
      ((pat1 pat2 ...) <<-: expr) 
      expr])]
Производит последовательные монадические вычисления. Соостветствует оператору @tt{do} в языке @emph{Haskell}.

Операторы @racket[_ops] могут иметь следующую форму:
@itemize{
  @item{@racketidfont{(pat @(defidform/inline <-) expr)} вычисляет выражение @racket[_expr] и производит сопоставление результата с шаблоном @racket[_pat] .}
  @item{@racketidfont{(pat @(defidform/inline <-:) expr)} эквивалентно @racket[(pat <- (return _expr))].}
  @item{@racketidfont{((pat1 pat2 ...) @(defidform/inline <<-) expr)} эквивалентно последовательности операторов @racket[(_pat1 <- _expr) (_pat2 <- _expr) ...].}
  @item{@racketidfont{((pat1 pat2 ...) @(defidform/inline <<-:) expr)} эквивалентно последовательности операторов @racket[(_pat1 <-: _expr) (_pat2 <-: _expr) ...].}
  @item{@racket[(_expr)] вычисляет выражение @racket[_expr] без связывания ради побочных эффектов.}}

@bold{Примеры:}

@def+int[#:eval formica-eval
 (define-formal f)]

Простое связывание:
@interaction[#:eval formica-eval
 (using List
   (do [x <- '(1 2 3)]
       [y <- '(a b c)]
       (return (f x y))))]

Использование сопоставления с образцом:
@interaction[#:eval formica-eval
 (using List
   (do [(cons x y) <- '((1 . 2) (3 . 4))]
       (return (f x y))))]


Здесь @racket[x] связывается со всем списком, а не с его элементами:
@interaction[#:eval formica-eval
 (using List
   (do [x <-: '(1 2 3)]
       [y <- '(a b c)]
       (return (f x y))))]

В этом случае символы @racket[x] и @racket[y] связываются с одним и тем же списком.
@interaction[#:eval formica-eval
 (using List
   (do [(x y) <<- '(1 2 3)]
       (return (f x y))))]

@interaction[#:eval formica-eval
 (using List
   (do [(x y) <<-: '(1 2 3)]
       [z <- x]
       (return (f x y z))))]

Пример вычисления с охраной:
@interaction[#:eval formica-eval
 (using List
   (do [(x y) <<- '(1 2 3)]
       (guard (< x y))
       (return (f x y))))]


@defform/subs[#:literals (<- <- <<- <<-:) 
  (collect expr ops ...+)
  ([ops (pat <- expr) 
        (pat <-: expr) 
        ((pat1 pat2 ...) <<- expr) 
        ((pat1 pat2 ...) <<-: expr) 
        guard-expr])]
Производит монадическую генерацию множеств. Определена для монад, имеющих нейтральный элемент.

Использует те же операторы, что и форма @racket[do], кроме охраняющих выражений: всякое выражение @racket[_guard-expr] вычисляется и используется, как аргумент функции @racket[guard].

Примеры:
@interaction[#:eval formica-eval
 (using List
   (collect (cons x y) [x <- '(1 2)] [y <- '(a b c)]))]
     
@interaction[#:eval formica-eval
 (using List
   (collect (cons x y) [(x y) <<- '(1 2 3)]))]
  
Использование охраны:
@interaction[#:eval formica-eval
 (using List
   (collect (cons x y) [(x y) <<- '(1 2 3)] (< x y)))]
  
@interaction[#:eval formica-eval
 (using List
   (collect `((gcd ,x ,y) = ,z)
     [(x y) <<- (range 8)]
     [z <- (range 2 x)]
     (= z (gcd x y))))]

@defproc[(undefined) undefined?]
Представляет объект, удовлетворяющий следующим соотношениям для любой монады:
@centered{@racket[(return (undefined)) ≡ (undefined)]
           
          @racket[(bind (undefined) >>= _f) ≡ (_f (undefined))]}
Объект @racket[(undefined)] можно использовать так же, как @emph{единичное выражение} @tt{()} в языке @emph{Haskell}.


@section[#:tag "monad:fun"]{Монадические функции и операторы}

@defproc[(return [x Any] ...) any]
Функция «возвращения» (втягивания) величин в монаду.

Примеры:
@interaction[#:eval formica-eval
 (using Id (return 5))
 (using List (return 1 2 3))]

@defthing[mzero Any]
Нейтральный (нулевой) элемент монады.

Примеры:
@interaction[#:eval formica-eval
 (using List mzero)
 (using Id mzero)]

@defproc[(mplus [x Any] [y Any]) Any]
Монадическая операция сложения.

Примеры:
@interaction[#:eval formica-eval
 (using List (mplus '(1 2 3) '(3 4 5)))]

@defproc[(failure [v Any]) any]
Функция, вызываемая в случае неудачи сопоставления с образцом в формах @racket[do] и @racket[collect].

Примеры:
@interaction[#:eval formica-eval
 (using Id (failure 'x))
 (using List (failure 'x))]

@defproc[(lift [f Fun]) Fun]
Возвращает функцию @racket[_f], втянутую в монаду. 

@centered{@racketblock[lift _f = return ∘ _f]}

Примеры:
@interaction[#:eval formica-eval
 (using List ((lift +) 1 2))]

@defform[(lift/m f arg ...+)]
Монадическая аппликация функции @racket[_f] к аргументам @racket[_arg ...].

@centered{@racketblock[(lift/m _f _a _b ...) ≡ (do [_x <- _a] 
                                                   [_y <- _b] 
                                                   ... 
                                                   [return (_f _x _y ...)])]}

Примеры:
@interaction[#:eval formica-eval
 (using List
   (lift/m cons '(a b c) '(x y)))]

@interaction[#:eval formica-eval
 (using List
   (lift/m or '(#t #f) '(#t #f)))]

@defform[(compose/m fs ...+)]
Монадическая композиция функций @racket[_fs].

@centered{@racketblock[(compose/m _f _g ...) ≡ 
                         (λ (x) 
                           (bind (return x) >>= _f >>= _g >>= ...))]}

Пример: в монаде @racket[List] возможна композиция неоднозначных функций:
@defs+int[#:eval formica-eval
 ((using-monad List)
  
  (define (Sqrt x) 
    (do (guard (positive? x))
        [r <-: (sqrt x)]
        (list r (- r)))))

 (Sqrt 4)
 ((compose/m Sqrt Sqrt) 16)]

@defproc[(guard [test Bool]) Any]
Оператор охраны. Определён для функций, имеющих нейтральный элемент.

@centered{@racket[(guard _test) ≡ (if _test (return (undefined)) mzero)]}

Примеры:
@interaction[#:eval formica-eval
 (using List
   (do [(x y) <<- '(1 2 3)]
       (guard (odd? x))
       (guard (< x y))
       (return (cons x y))))]

Используя @racket[guard] можно реализовать поиск с возвратом:

@interaction[#:eval formica-eval
 (define tell (lift printf))
 (using List
   (collect (cons x y)
     [x <- '(1 2 3 4)]
     (tell "x: ~a\n" x)
     (odd? x)
     [y <- '(1 2 3)]
     (tell "x: ~a\ty: ~a\n" x y)
     (< x y)))]

@defproc[(guardf [pred unary?]) any]
Охраняющая функция.  Определена для функций, имеющих нейтральный элемент.

@centered{@racket[(guardf _pred) = (bind (guard (_pred _x)) >> (return _x))]}

Примеры:
@interaction[#:eval formica-eval
 (using List
   (bind '(1 2 3) >>= (guardf odd?) >>= (lift sqr)))]

@defproc[(sequence/m [lst list?]) Any]
Функция для последовательного выполнения списка действий, обёрнутых монадой. В качестве результата возвращает список значений, обёрнутый исходной монадой.

@racketblock[sequence/m empty-stream = (return '())
             sequence/m (scons a as) = (lift/m cons a (sequence/m as))]

Примеры:
@interaction[#:eval formica-eval
 (using List
   (sequence/m '((a) 3 (x y))))]

@interaction[#:eval formica-eval
 (using Stream
   (stream-first 
    (sequence/m (list '(a) 3 (stream 'x (/ 0))))))]

@defproc[(map/m [f unary?] [lst list?]) Any]
Монадическое отображение.

@racketblock[map/m _f = sequence/m ∘ (map _f)]

Пример (определение функции @racketidfont{Sqrt} приводится в примере к оператору @racket[compose/m]):
@interaction[#:eval formica-eval                  
 (using List
   (map/m Sqrt '(1 4 9)))]

@defproc[(fold/m [f binary?] [x0 Any] [lst list?]) Any]
Монадическая свёртка.

@racketblock[(fold/m _f _x0 '()) = (return _x0)
(fold/m _f _x0 (cons _h _t)) = (do [_y <- (_f _x0 _h)] 
                                   (fold-m _f _y _t))]

Примеры:
@interaction[#:eval formica-eval
 (using List
   (fold/m (lift (hold +)) 0 '(1 2 3)))
 (using List
   (fold/m (λ(x y) (list (($ +) x y) 
                         (($ -) x y))) 
           0 
           '(1 2 3)))]

@defproc[(filter/m [f unary?] [x0 Any] [lst list?]) Any]
Монадический фильтр.

@racketblock[(filter/m _pred '()) = (return '())
(filter/m _pred (cons _h _t)) = (do [_b <- (_pred _h)]
                                    [_x <- (filter/m _pred _t)]
                                    (return (if _b (cons _h _x) _x)))]

Примеры:
@interaction[#:eval formica-eval
 (using List
   (filter/m (lift odd?) '(1 2 3 4 5)))
 (using List
   (filter/m (λ(x) (list #t #f)) '(1 2 3)))]

@defproc[(sum/m [lst list?]) Any]
Монадическая сумма, определена для аппликативных монад.

@centered{@racket[(sum/m _lst) = (foldr mplus mzero _lst)]}

Примерыы:
@interaction[#:eval formica-eval
 (using List
   (sum/m '((1 2 3) (2 3 4) '(a b))))]