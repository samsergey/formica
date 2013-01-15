#lang scribble/doc

@(require (for-label formica))

@(require
  scribble/manual
  scribble/eval)

@(define formica-eval
   (let ([sandbox (make-base-eval)])
     (sandbox '(require formica))
     sandbox))

@title[#:tag "grammar"]{Грамматика и синтаксис}

Синтаксической единицей языка является @emph{S-выражение}. 

S-выражение может быть либо @emph{атомом}, либо @emph{списком}, содержащим S-выражения.

@(racketgrammar S-expr atom (S-expr ...))

Ниже приводится формальная грамматика языка Formica.

@(racketgrammar* #:literals (quote quasiquote unquote unquote-splicing ? or and _ ___ --> -->. do collect using 
                                   define define/c define/. define//. define/memo define-formal define-monad
                                   <- <-: <<- <<-: /. //. delay) 
                 [program definition 
                          expr...]
                 [definition (define spec expr ...)
                   (define/c spec expr ...)
                   (define/. spec expr ...)
                   (define//. spec expr ...)
                   (define/memo spec expr ...)
                   (define-formal frm ...)
                   (define-monad id expr)
                   (define-syntax-rule spec expr)]
                 [spec id (id id ...)]
                 [frm id (id number)]
                 [expr datum
                       fun-expr
                       monadic-expr
                       literal
                       delayed-expr]
                 [fun-expr function 
                           (f expr ...) 
                           rewriting]
                 [monadic-expr (using spec expr ...)
                               (do op ...) 
                               (collect expr op ...)]
                 [op (patt <- expr)
                     (patt <-: expr)
                     (patt <<- expr)
                     (patt <<-: expr)
                     expr]
                 [datum boolean number symbol string]
                 [f form fun-expr]
                 [literal '(expr ...) 
                          `(q-expr ...)]
                 [delayed-expr (delay expr)]
                 [rewriting  
                       (/. [patt ... --> expr] ...)
                       (//. [patt ... <arr> expr] ...)]
                 [q-expr expr ,expr ,@expr]
                 [patt datum id (id patt ...) (? f [id]) (or patt ...) (and patt ...) _ __k ____]
                 [<arr> --> -->.])

@itemize{
@item{@emph{Программа} является последовательностью @emph{определений} @racket[_definition] и @emph{выражений} @racket[_expr].}

@item{@emph{Определением} @racket[_definition] могут быть @emph{определения функций} @racket[define], @racket[define/c], @racket[define/.], @racket[define//.], @racket[define/memo],  @emph{определения формальных функций} @racket[define-formal], @emph{определения монад} @racket[define-monad] и @emph{определение синтаксических форм} @racket[define-syntax-rule].} 
 
@item{@emph{Выражением} @racket[_expr] могут быть @emph{элементарные данные} @racket[_datum], 
@emph{функциональное выражение} @racket[_fun-expr], @emph{монадическое вычисление} @racket[_monad-expr], 
@emph{квотированное выражение} @racket[_literal], 
@emph{отложенное вычисление} @racket[_delayed-expr].}

@item{@emph{Функциональным выражением} @racket[_fun-expr] могут быть @emph{имя функции} @racket[_function], 
@emph{аппликация} @racket[(_f _expr ...)], или
@emph{подстановка} @racket[_rewrite].}

@item{@emph{Монадическое вычисление} @racket[_monadic-expr] может быть 
       @emph{указание на монаду} @racket[(using ...)],
       @emph{последовательностью вычислений} @racket[(do ...)], 
       или генератором последовательности @racket[(collect ...)].}

@item{Элементарные данные могут быть
 @itemize{
   @item{величиной логического типа @racket[#t] или @racket[#f], обозначающие,
         соответственно, истинное и ложное значение.}
   @item{@emph{числом}
    @itemize{
     @item{целым --- @racket[123], @racket[-1];}
     @item{рациональным --- @racket[3/2], @racket[-1/23];}
     @item{с плавающей точкой --- @racket[1.23], @racket[-4.56e-17];}
     @item{комплексным --- @racket[1+2i], @racket[1/2-3/4i], @racket[1.2-3.45i].}}}
   @item{@emph{символом} --- последовательностью знаков, не включающих пробел и
   @litchar{" , ' ` ( ) [ ] { } | ; #}.}
   @item{@emph{строкой} --- последовательностью знаков, ограниченной символами @litchar{"}, 
     например, @racket["abc"], @racket["a sentence\n"].}}}

@item{Аппликация --- это применение к последовательности аргументов @racket[_expr ...]
  @emph{специальной формы} @racket[_form] или любого выражения,
 которое, будучи вычисленным, возвращает функцию @racket[_fun-expr].}

@item{@emph{Квотированное выражение} @racket[_literal] --- это невычисляемое S-выражение, 
       образованное с помощью кавычек @litchar{' `} или 
       форм @racket[quote] и @racket[quasiquote].
       
       В форме  @racket[quasiquote] возможно вычисление частей выражения,
       отмеченных с помощью символов @litchar{,} и @litchar{@"@",}.}


@item{@emph{Подстановка} @racket[_replace] определяет систему правил переписывания, использующих
       @elemref["t:patt"]{@emph{шаблоны}} @racket[_patt] и @emph{стрелки} @racket[_<arr>].}

}