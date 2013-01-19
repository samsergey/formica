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
                                   define define/c define/. define//. define/memo define-formal define-monad define-syntax-rule require test libname
                                   ==> =error=> <- <-: <<- <<-: /. //. delay exn-pred name if cond boolean number symbol string formal) 
                 [program def-or-expr...]
                 [def-or-expr 
                   definition 
                   lib-require
                   expr]
                 [definition 
                   (define spec expr ...+)
                   (define/c spec fun-abstr)
                   (define/. spec [patt ...+ --> expr] ...+)
                   (define//. spec [patt ...+ <arr> expr] ...+)
                   (define/memo spec expr ...+)
                   (define-formal frm ...+)
                   (define-monad name [#:kwd expr] ...+)
                   (define-syntax-rule spec expr)]
                 [spec name (spec arg ...)]
                 [arg name (name expr) (#:kwd name) (#:kwd name expr)]
                 [frm name (name number)]
                 [lib-require (require lib ...+)]
                 [lib "filepath" libname]
                 [expr datum 
                       fexpr]
                 [datum boolean number symbol string literal]
                 [f-expr name
                         fun-abstr
                         fun-app
                         local-binding
                         control
                         monadic-expr
                         tests]
                 [literal '(expr ...) 
                          `(q-expr ...)]
                 [q-expr expr ,expr ,@expr]
                 [fun-abstr (λ spec expr ...+)
                            (/. [patt ...+ --> expr] ...+)
                            (//. [patt ...+ <arr> expr] ...+)]
                 [patt datum 
                       name 
                       (name patt ...)
                       (formal patt)
                       (? f [name]) 
                       (or patt ...+) 
                       (and patt ...+) 
                       _ 
                       __k 
                       ____]
                 [<arr> --> -->.]
                 [fun-app (f-expr expr ...)]
                 [local-binding (let ([name expr] ...) expr)
                                (let name ([name expr] ...) expr)
                                (let* ([name expr] ...) expr)]
                 [control (if expr expr expr)
                          (cond [expr expr] ...)
                          (cond [expr expr] ... [else expr])
                          (delay expr)]
                 [monadic-expr (using spec expr ...)
                               (do op ...) 
                               (collect expr op ...)]
                 [op (patt <- expr)
                     (patt <-: expr)
                     (patt <<- expr)
                     (patt <<-: expr)
                     expr]
                 [tests (test test-case ...)]
                 [test-case (expr ==> expr)
                            (expr =error=> exn-pred)
                            expr])

@itemize{
@item{@emph{Программа} является последовательностью @emph{определений} @racket[_definition], @emph{выражений} @racket[_expr] или @emph{зависимостей} @racket[_lib-require].}
 
@item{@emph{Выражением} @racket[_expr] могут быть @emph{элементарные данные} @racket[_datum] или @emph{функциональное выражение} @racket[_fun-expr].}
@item{@emph{Функциональным выражением} @racket[_fun-expr] могут быть @emph{имя функции} @racket[_name], @emph{абстракция} @racket[_fun-abstr], @emph{аппликация} @racket[_fun-app], @emph{локальное связывание} @racket[_local-binding], @emph{управляющая конструкция} @racket[_control], @emph{монадическое вычисление} @racket[_monad-expr] или набор тестов @racket[_tests].}

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
     например, @racket["abc"], @racket["a sentence\n"].}}}}