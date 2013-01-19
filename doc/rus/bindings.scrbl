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

@title[#:tag "bindings"]{Создание функций}

В этом разделе описываются способы создания и определения функций и синтаксических форм в языке Formica.

@local-table-of-contents[]

@section[#:tag-prefix "regular"]{Определения и связывание}

@defform*[[(define x body)
           (define (f var ...) body ...)
           (define (f var ... . rest) body ...)
           (define ((f var1 ...) var2 ...) body ...)]]

Оператор связывания.

@itemize{@item{@racket[(define _x _body)] ---
связывает символ @racket[_x] с результатом вычисления выражения @racket[_body].

Примеры:
@interaction[#:eval formica-eval
  (define x 4)
  x
  (define y x)
  y]

Определения функций с помощью @elemref["t:partial"]{частичного применения}:
@interaction[#:eval formica-eval
  (define 1+ (+ 1))
  1+
  (1+ 2)
  (1+ 2 3)]

@interaction[#:eval formica-eval
  (define odds (filter odd?))
  (odds '(1 2 3 4 5))]}}


@itemize{@item{@racket[(define (_f _var ...) _body ...)] ---
определяет функцию с именем @racket[_f], принимающую формальные аргументы @nonbreaking{@racket[_var ...]}, 
как результат вычисления последнего из выражений @racket[_body ...].

Примеры:
@interaction[#:eval formica-eval
  (define (f x) (* 2 x))
  (f 3)]
@interaction[#:eval formica-eval
  (define (g x) 
    (define a 4) 
    (* a x))
  (g 3)]}}

@itemize{@item{@racket[(define (_f _var ... . _rest) _body ...)] ---
подобна предыдущей форме, но определяемая функция может принимать произвольное число формальных аргументов. При этом фактические аргументы  @racket[_var ...] должны быть заданы обязательно, а  @racket[_rest] связывается со списком прочих аргументов.

Примеры:
@interaction[#:eval formica-eval
  (define (f x y . z) (list x y z))
  (f 1 2 3 4)
  (f 1 2 3)
  (f 1 2)
  (f 1)]}}

@itemize{@item{@racket[(define ((_f _var1 ...) _var2 ...) _body ...)] ---
определяет  @elemref["t:curry"]{каррированную} функцию с фиксированными аргументами  @racket[_var1 ...].

Примеры:
@interaction[#:eval formica-eval
  (define ((f x y) z t) (list x y z t))
  (f 1 2)
  ((f 1 2) 3 4)]}}

@defform[(define-syntax-rule (form stx ...) expr)]
Определяет @elemref["t:form"]{синтаксическую форму} @racket[_form], которая преобразуется в 
выражение @racket[_expr] до его вычисления.

Примеры:
@interaction[#:eval formica-eval
  (define-syntax-rule (when p expr) 
    (if p expr (display "Fail!")))
  (when (odd? 2) (display "test"))
  (when (odd? 3) (display "test"))]

Если бы мы определили @racket[when], как функцию, сначала были бы вычислены 
все её фактические аргументы, а потом был бы сделан выбор:

@interaction[#:eval formica-eval
  (define (when p expr) 
    (if p expr (display "Fail!")))
  (when (odd? 2) (display "test"))
  (when (odd? 3) (display "test"))]

@defform[(require name ...)]
Загружает определения, экспортируемые модулями @racket[_name ...].

@defform[(provide id ...)]
Экспортирует определения для символов и функций @racket[_id ...] из данного модуля.

@section[#:tag-prefix "local-binding"]{Локальное связывание}

@defform*[[(let ([x expr] ...) body)
           (let f ([x expr] ...) body)]]

Определение локальных переменных и функций.

@itemize{@item{@racket[(let ([_x _expr] ...) _body)] --- определяет локальные переменные, связывая символы @racket[_x ...] 
со значениями соответствующих выражений @racket[_expr ...], и вычисляет  @racket[_body],
используя эти переменные.

Примеры:
@interaction[#:eval formica-eval
  (define x 5)
  (define y 8)
  (let ([x 2] [y 3]) (list x y))
  (list x y)]}}

@itemize{@item{@racket[(let _f ([_x _expr] ...) _body)] ---
определяет функцию @racket[_f] с телом @racket[_body] и формальными
аргументами @racket[_x ...], после чего немедленно вызывает её 
с фактическими аргументами @racket[_expr ...].

Используется для вычисления локально определяемых рекурсивных функций.

Примеры:
@interaction[#:eval formica-eval
  (let fact ([n 4])
    (if (zero? n)
        1
        (* n (fact (- n 1)))))
  (let fact ([n 4] [res 1])
    (if (zero? n)
        res
        (fact (- n 1) (* n res))))]}}


@section[#:tag-prefix "point-free"]{Cвязывание в бесточечной нотации}


@defform/subs[#:literals (λ /. //.) (define/c (f var ...) rhs) 
([rhs
   fun-constructor
   (g args ...)])]

Оператор связывания в бесточечной нотации, определяет функцию @racket[_f]. Правая часть определения @racket[_rhs] может быть либо конструктором функции или подстановки @racket[_fun-constructor], либо частичным вызовом функции @racket[_g] с фиксированными аргументами @racket[_arg ...]. Формальными аргументами определяемой функции являются аргументы левой части определения  @racket[_var ...], дополненные свободными аргументами правой части определения.

Примеры:
@interaction[#:eval formica-eval
  (define/c (map1 f) 
    (/. (cons h t) --> (cons (f h) (map1 f t))))
  (procedure-arity map1)
  (map1 ($ 'f) '(a b c))]
@interaction[#:eval formica-eval
  (define/c (map2 f) (foldr (∘ cons f) '()))
  (procedure-arity map2)
  (map2 ($ 'f) '(a b c))
  (map2 ($ 'g 2) '(a b c) '(x y z))]



@include-section["formal.scrbl"]

@include-section["rewrite.scrbl"]