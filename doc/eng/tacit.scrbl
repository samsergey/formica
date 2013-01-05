#lang scribble/doc

@(require (for-label formica))

@(require
  scribble/manual
  scribble/eval)

@(define formica-eval
   (let ([sandbox (make-base-eval)])
     (sandbox '(require formica))
     sandbox))

@title[#:tag "tacit"]{Point-free function definitions}

@declare-exporting[formica/tacit]
The bindings documented in this section are provided by the @racket[formica/tacit] library and @racket[formica] language.

@defform/subs[#:literals (lambda match-lambda case-lambda rewrite rewrite-all rewrite-repeated rewrite-all-repeated /. //.) 
                         (define/c (f var ...) rhs) 
([rhs
   fun-constructor
   (g args ...)
   lit]
 [fun-constructor
   (lambda arg-spec body)
   (match-lambda arg-spec body) 
   (case-lambda arg-spec body) 
   (rewrite rules ...)
   (rewrite-all rules ...)
   (rewrite-repeated rules ...)
   (rewrite-all-repeated rules ...)
   (//. rules ...)
   (/. rules ...)])]

Defines a function @racket[_f] in the @deftech{point-free notation}. The right-hand side 
of the definition @racket[_rhs] can be either a function constructor @racket[_fun-constructor], 
partially applied function @racket[_g] with fixed arguments @racket[args ...], or any literal expression @racket[_lit].
The defined function @racket[_f] has formal arguments @racket[_var ...], complemented by free arguments of
the right right-hand side @racket[_rhs], if any.


Examples:
@interaction[#:eval formica-eval
  (define/c (F x)
    (case-lambda
      [(y) `(F ,y ,x)]
      [(y z) `(F ,x ,y ,z)]))
  (procedure-arity F)
  (F 1 2)
  (F 1 2 3)]
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
@interaction[#:eval formica-eval
  (define/c (f x) `(,x ,x))
  (procedure-arity f)
  (f 'a)
  (f '(a b c))]