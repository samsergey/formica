#lang scribble/doc

@(require (for-label formica))

@(require 
  (except-in scribble/manual ::)
  scribble/eval)

@(define formica-eval
   (let ([sandbox (make-base-eval)])
     (sandbox '(require formica)) 
     sandbox))

@title[#:tag "contracts"]{Function signatures}

The @deftech{function signature} defines the domain and range of a function. On the other hand the signature plays role of the contract: it defines preconditions and postconditions for a function in terms of predicates which should be satisfied by arguments and a result. In case of pure functions this two interpretations of signature coincide. 

@defform/subs[#:literals (? ..)
   (-> dom range)
   ([dom (type ...)
         (type ... rest ..)
         (type ... (? opt ...))
         (type ... (? opt ...) rest ..)])]
Defines a function signature, having domain @racket[_dom] and range  @racket[_range]. Function may have optional arguments @racket[_opt]. For @tech{variadic} functions the type of arguments is given by the contract @racket[_rest] followed by symbol @litchar{..} .

Could be used in the infix notation:
@racketblock[(dom -> range)]


@defform[(:: f sig fun-def)]
Binds the signature @racket[_sig] with the function @racket[_f], defined by @racket[_fun-def]. Signature may include unbounded symbols which are interpreted as polymorphic types. 

The definition @racket[_fun-def] could be given using any binding form:
@racket[define], @racket[define/c], @racket[define/.], @racket[define//.], @racket[define/memo] or @racket[define/memo/c].

@bold{Examples}

Definition of numeric function:
@def+int[#:eval formica-eval
  (:: sqr (Num -> Num)
   (define (sqr x)
     (* x x)))
   (sqr 3)
   (sqr 'a)]

This defines a function, which must have a positive number and a symbol as first and second arguments, resulting an even number.
@def+int[#:eval formica-eval
  (:: f (positive? Sym -> even?)
   (define (f x s)
     (* 2 x)))
   (f 3 'a)
   (f -3 'a)
   (f 3/2 'a)]

The signature of variadic function:
@def+int[#:eval formica-eval
  (:: append (list? list? .. -> list?)
   (define/. append
     a --> a
     a '() --> a
     a b --> (foldr cons b a)
     a b ... --> (append a (apply append b))))
   (append '(1 2 3) '(a b c) '(x y))
   (append '(a b c))
   (append '(a b c) 5)]

Following function has first argument of functional type:
@def+int[#:eval formica-eval
   (:: any ((Num -> Bool) (list: Num ..) -> Bool)
     (define/c (any pred)
       (foldr (∘ or pred) #f)))
   (any odd? '(1 2 3 4))
   (any odd? '(0 2 8 4))
   (any + '(0 2 8 4))
   (any odd? '(0 x 8 4))]

The signature of function having optional arguments:
@def+int[#:eval formica-eval
  (:: range (Real (? Real Real) -> (list: Real ..)) 
    (define range
      (case-lambda
        [(n) (range 1 n)]
        [(a b) (range a b 1)]
        [(a b s) (let loop ([i a] [res '()])
                   (if (< (- b i (- s)) s)
                       (reverse res)
                       (loop (+ i s) (cons i res))))])))
  (range 4)
  (range 3 8)
  (range -1 1 1/2)]
   
@bold{Polymorphic types}

The following signature uses polymorphic types @racket[A] and @racket[B].

@def+int[#:eval formica-eval
   (:: bind (A (A -> B) -> B)
     (define (bind x f)
       (f x)))
   (bind 4 sqrt)
   (bind 's cons)]

Here is the signature of the @racket[foldr] function:

@def+int[#:eval formica-eval
  (:: foldr ((a b -> b) b (list: a ..) -> b)
    (define/c (foldr f x0)
      (/. '() --> x0
          (cons h t) --> (f h (foldr f x0 t)))))
  (foldr ($ 'f) 'x0 '(1 2 3))
  (foldr + 0 '(1 2 3))]