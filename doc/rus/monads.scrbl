#lang scribble/doc

@(require (for-label formica))

@(require
  scribble/manual
  scribble/eval)

@(define formica-eval
   (let ([sandbox (make-base-eval)])
     (sandbox '(require formica))
     sandbox))

@title[#:tag "monads"]{Монады}

@declare-exporting[formica]

@elemtag["t:monad"]{@emph{Монада}} является абстракцией линейной цепочки связанных вычислений. Она определяется @emph{единичной функцией} @tt{return} и @emph{операцией связывания} @tt{>>=} (@tt{bind}), для которых выполняются следующие отношения:

@racketblock[
    _m >>= return  =  _m
(return _x) >>= _f  =  (_f _x)
   _m >>= _f >>= _g  =  _m >>= (λ (_x) ((_f _x) >>= _g))]
                                               
Для аддитивных монад определен @emph{нулевой элемент} @tt{mzero}, такой, что
@racketblock[
                     mzero >>= _f  =  mzero
             _x >>= (λ (_y) mzero)  =  mzero] 
и операция @emph{монадического сложения} @tt{mplus}, для которой @tt{mzero} является
нейтральным элементом:
@racketblock[mplus mzero _x  =  _x
             mplus _x mzero  =  _x] 
                                   
@local-table-of-contents[]

@include-section["monads-base.scrbl"]
@include-section["monads-List.scrbl"]