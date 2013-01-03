4550
#hash(((def ((lib "formica/tools.rkt") all-args)) . ((3771 . 3))) ((def ((lib "formica/formal.rkt") n/f-list?)) . ((2803 . 5))) ((def ((lib "formica/main.rkt") Num)) . ((145 . 2))) ((def ((lib "formica/tools.rkt") function?)) . ((3135 . 3))) ((def ((lib "formica/arity.rkt") unary?)) . ((4794 . 3))) ((def ((lib "formica/arity.rkt") binary?)) . ((4844 . 3))) ((def ((lib "formica/tags.rkt") tag?)) . ((4260 . 3))) ((def ((lib "formica/tools.rkt") arg)) . ((3225 . 3))) ((form ((lib "formica/main.rkt") ::)) . ((1107 . 2))) ((form (#<path:/home/serpol/dev/racket/formica/main.rkt> rewrite-all)) . ((1957 . 2))) ((def ((lib "formica/main.rkt") Fun)) . ((316 . 2))) ((def ((lib "formica/tools.rkt") fif)) . ((3503 . 5))) ((def ((lib "formica/tools.rkt") andf)) . ((3579 . 4))) ((def ((lib "formica/arity.rkt") variadic?)) . ((4636 . 3))) ((def ((lib "formica/tags.rkt") set-tag)) . ((4377 . 4))) ((def ((lib "formica/tools.rkt") curried?)) . ((4032 . 3))) ((def ((lib "formica/arity.rkt") inherit-arity)) . ((5033 . 3))) ((def ((lib "formica/tools.rkt") curry)) . ((3865 . 7))) ((def ((lib "formica/arity.rkt") min-arity)) . ((4895 . 3))) ((def ((lib "formica/memoize.rkt") memoized)) . ((2920 . 3))) ((form ((lib "formica/main.rkt") define-type)) . ((340 . 6))) ((def ((lib "formica/tools.rkt") fork)) . ((3708 . 4))) ((def ((lib "formica/tools.rkt") fixed-point)) . ((4084 . 4))) ((def ((lib "formica/main.rkt") Nat)) . ((218 . 2))) ((def ((lib "formica/formal.rkt") formal-function?)) . ((2678 . 3))) ((def ((lib "formica/memoize.rkt") memoized?)) . ((2973 . 3))) ((form ((lib "formica/main.rkt") is)) . ((49 . 5))) ((form (#<path:/home/serpol/dev/racket/formica/main.rkt> rewrite-repeated)) . ((2060 . 9))) ((def ((lib "formica/formal.rkt") n/f-pair?)) . ((2803 . 5))) ((def ((lib "formica/formal.rkt") formal?)) . ((2734 . 3))) ((def ((lib "formica/main.rkt") contract?)) . ((0 . 3))) ((form ((lib "formica/memoize.rkt") define/memo)) . ((3022 . 3))) ((def ((lib "formica/tags.rkt") check-tag)) . ((4307 . 4))) ((def ((lib "formica/main.rkt") Real)) . ((169 . 2))) ((form (#<path:/home/serpol/dev/racket/formica/main.rkt> /.)) . ((1994 . 2))) ((form ((lib "formica/main.rkt") ->)) . ((868 . 10))) ((def ((lib "formica/tools.rkt") const)) . ((3267 . 3))) ((def ((lib "formica/main.rkt") not/c)) . ((625 . 3))) ((def ((lib "formica/tools.rkt") orf)) . ((3644 . 4))) ((def ((lib "formica/main.rkt") cons:)) . ((681 . 4))) ((def ((lib "formica/formal.rkt") hold)) . ((2545 . 4))) ((form (#<path:/home/serpol/dev/racket/formica/main.rkt> rewrite)) . ((1789 . 6))) ((def ((lib "formica/main.rkt") Any)) . ((456 . 3))) ((form ((lib "formica/formal.rkt") formal-out)) . ((2891 . 2))) ((def ((lib "formica/main.rkt") Str)) . ((268 . 2))) ((def ((lib "formica/main.rkt") Sym)) . ((292 . 2))) ((def ((lib "formica/main.rkt") Index)) . ((242 . 2))) ((def ((lib "formica/tools.rkt") any-args)) . ((3818 . 3))) ((def ((lib "formica/main.rkt") and/c)) . ((564 . 3))) ((def ((lib "formica/main.rkt") Bool)) . ((120 . 2))) ((def ((lib "formica/tools.rkt") negated)) . ((3411 . 3))) ((form ((lib "formica/main.rkt") define/c)) . ((1203 . 16))) ((def ((lib "formica/tags.rkt") tagged?)) . ((4210 . 3))) ((form ((lib "formica/formal.rkt") formal)) . ((2781 . 2))) ((form ((lib "formica/formal.rkt") define-formal)) . ((2405 . 8))) ((form (#<path:/home/serpol/dev/racket/formica/main.rkt> rewrite-all-repeated)) . ((2291 . 2))) ((def ((lib "formica/main.rkt") or/c)) . ((504 . 3))) ((def ((lib "formica/tools.rkt") flipped)) . ((3457 . 3))) ((def ((lib "formica/tools.rkt") greedy)) . ((3366 . 3))) ((form (#<path:/home/serpol/dev/racket/formica/main.rkt> //.)) . ((2337 . 2))) ((form ((lib "formica/main.rkt") list:)) . ((764 . 5))) ((def ((lib "formica/tools.rkt") curryr)) . ((3865 . 7))) ((def ((lib "formica/main.rkt") Int)) . ((194 . 2))) ((form (#<path:/home/serpol/dev/racket/formica/main.rkt> define/.)) . ((2022 . 2))) ((def ((lib "formica/arity.rkt") polyadic?)) . ((4689 . 3))) ((def ((lib "formica/tools.rkt") composition)) . ((3311 . 3))) ((def ((lib "formica/partial-app.rkt") apply*)) . ((1136 . 4))) ((form (#<path:/home/serpol/dev/racket/formica/main.rkt> define//.)) . ((2366 . 2))) ((def ((lib "formica/arity.rkt") max-arity)) . ((4960 . 3))) ((def ((lib "formica/tools.rkt") id)) . ((3184 . 3))) ((def ((lib "formica/tags.rkt") set-tag*)) . ((4480 . 4))) ((def ((lib "formica/arity.rkt") fixed-arity?)) . ((4580 . 3))) ((form ((lib "formica/memoize.rkt") define/memo/c)) . ((3091 . 2))) ((def ((lib "formica/arity.rkt") nullary?)) . ((4742 . 3))))
procedure
(contract? v) -> Bool
  v : Any
syntax
(is v type-pred)
 
  v : Any
  type-pred : contract?
value
Bool : contract?
value
Num : contract?
value
Real : contract?
value
Int : contract?
value
Nat : contract?
value
Index : contract?
value
Str : contract?
value
Sym : contract?
value
Fun : contract?
syntax
(define-type name c ...)
(define-type (name x ...) c ...)
 
  c : contract?
  x : contract?
procedure
(Any v) -> contract?
  v : Any
procedure
(or/c c ...) -> contract?
  c : contract?
procedure
(and/c c ...) -> contract?
  c : contract?
procedure
(not/c c) -> contract?
  c : contract?
procedure
(cons: c1 c2) -> contract?
  c1 : contract?
  c2 : contract?
syntax
(list: c ...)
(list: c ..)
 
  c : contract?
syntax
(f: c ...)
 
  c : contract?
syntax
(-> dom ... range)
(-> dom ... rest .. range)
(-> dom ... (? opt ...) range)
(-> dom ... (? opt ...) rest .. range)
 
  dom : contract?
  range : contract?
  opt : contract?
  rest : contract?
syntax
(:: f sig fun-def)
procedure
(apply* f v ...) -> Any
  f : Fun
  v : Any
syntax
(define/c (f var ...) rhs)
 
rhs             = fun-constructor
                | (g args ...)
                | lit
                   
fun-constructor = (lambda arg-spec body)
                | (match-lambda arg-spec body)
                | (case-lambda arg-spec body)
                | (rewrite rules ...)
                | (rewrite-all rules ...)
                | (rewrite-repeated rules ...)
                | (rewrite-all-repeated rules ...)
                | (//. rules ...)
                | (/. rules ...)
syntax
(rewrite rule-spec ...)
 
rule-spec = (pat ... --> expr)
          | (pat ... --> (=> id) expr)
          | (pat ... --> (? guard) expr)
syntax
(rewrite-all rule-spec ...)
syntax
(/. rule-spec ...)
syntax
(define/. id rule-spec ...)
syntax
(rewrite-repeated rule-spec ...)
 
rule-spec = (pat ... ar expr)
          | (pat ... ar (=> id) expr)
          | (pat ... ar (? guard) expr)
             
ar        = -->
          | -->.
syntax
(rewrite-all-repeated rule-spec ...)
syntax
(//. rule-spec ...)
syntax
(define//. id rule-spec ...)
syntax
(define-formal f-spec ...)
 
f-spec = id
       | (id arity-spec)
 
  id : symbol?
  arity-spec : procedure-arity?
procedure
(hold f [arity]) -> formal-function?
  f : (or/c Fun Sym)
  arity : procedure-arity? = (arity-at-least 0)
procedure
(formal-function? x) -> Bool
  x : Any
procedure
(formal? x) -> Bool
  x : Any
syntax
(formal patt)
procedure
(n/f-pair? x) -> Bool
  x : Any
(n/f-list? x) -> Bool
  x : Any
syntax
(formal-out id ...)
procedure
(memoized f) -> memoized?
  f : Fun
procedure
(memoized? f) -> Bool
  f : Fun
syntax
(define/memo (f var ...) body ...)
(define/memo f fun)
syntax
(define/memo/c (f var ...) body)
procedure
(function? x) -> Bool
  x : Any
procedure
(id x) -> Any
  x : Any
procedure
(arg n) -> Fun
  n : Ind
procedure
(const x) -> Fun
  x : Any
procedure
(composition f ...) -> Fun
  f : Fun
procedure
(greedy f) -> Fun
  f : Fun
procedure
(negated p) -> Fun
  p : Fun
procedure
(flipped f) -> Fun
  f : Fun
procedure
(fif p f g) -> Fun
  p : Fun
  f : Fun
  g : Fun
procedure
(andf f g ...) -> Fun
  f : Fun
  g : Fun
procedure
(orf f g ...) -> Fun
  f : Fun
  g : Fun
procedure
(fork f g) -> Fun
  f : Fun
  g : unary?
procedure
(all-args p) -> Fun
  p : Fun
procedure
(any-args f) -> Fun
  f : Fun
procedure
(curry f arg ...) -> (or/c curried? Any)
  f : Fun
  arg : Any
(curryr f arg ...) -> (or/c curried? Any)
  f : Fun
  arg : Any
procedure
(curried? x) -> boolean?
  x : Any
procedure
(fixed-point f [#:same-test same?]) -> Fun
  f : Fun
  same? : (any/c any/c -> boolean?) = equal?
procedure
(tagged? f) -> tagged?
  f : Fun
procedure
(tag? f) -> Sym
  f : tagged?
procedure
(check-tag f t) -> Bool
  f : tagged?
  t : Any
procedure
(set-tag t [n]) -> (Fun -> tagged?)
  t : Sym
  n : (or/c symbol? #f) = #f
procedure
(set-tag* t [n]) -> (Fun -> Fun)
  t : Sym
  n : (or/c symbol? #f) = #f
procedure
(fixed-arity? f) -> boolean?
  f : Any
procedure
(variadic? f) -> boolean?
  f : Any
procedure
(polyadic? f) -> boolean?
  f : Any
procedure
(nullary? f) -> boolean?
  f : Any
procedure
(unary? f) -> boolean?
  f : Any
procedure
(binary? f) -> boolean?
  f : Any
procedure
(min-arity f) -> (or/c 0 positive?)
  f : Fun
procedure
(max-arity f) -> (or/c 0 positive? +inf.0)
  f : Fun
procedure
(inherit-arity f) -> (Fun -> Fun)
  f : Fun
