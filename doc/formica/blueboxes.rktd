3193
#hash(((def ((lib "formica/tools.rkt") flipped)) . ((2106 . 3))) ((def ((lib "formica/arity.rkt") min-arity)) . ((3544 . 3))) ((def ((lib "formica/tools.rkt") arg)) . ((1874 . 3))) ((def ((lib "formica/formal.rkt") n/f-list?)) . ((1667 . 5))) ((def ((lib "formica/tags.rkt") tagged?)) . ((2859 . 3))) ((def ((lib "formica/tools.rkt") curry)) . ((2514 . 7))) ((def ((lib "formica/tools.rkt") all-args)) . ((2420 . 3))) ((def ((lib "formica/formal.rkt") formal?)) . ((1598 . 3))) ((def ((lib "formica/tools.rkt") negated)) . ((2060 . 3))) ((form ((lib "formica/formal.rkt") formal)) . ((1645 . 2))) ((def ((lib "formica/tools.rkt") id)) . ((1833 . 3))) ((form ((lib "formica/formal.rkt") formal-out)) . ((1755 . 2))) ((def ((lib "formica/tools.rkt") fork)) . ((2357 . 4))) ((form ((lib "formica/main.rkt") define/c)) . ((67 . 16))) ((def ((lib "formica/tools.rkt") orf)) . ((2293 . 4))) ((def ((lib "formica/tools.rkt") const)) . ((1916 . 3))) ((form (#<path:/home/serpol/dev/racket/formica/main.rkt> rewrite)) . ((653 . 6))) ((def ((lib "formica/arity.rkt") binary?)) . ((3493 . 3))) ((def ((lib "formica/tools.rkt") fixed-point)) . ((2733 . 4))) ((form (#<path:/home/serpol/dev/racket/formica/main.rkt> rewrite-repeated)) . ((924 . 9))) ((form (#<path:/home/serpol/dev/racket/formica/main.rkt> rewrite-all-repeated)) . ((1155 . 2))) ((form (#<path:/home/serpol/dev/racket/formica/main.rkt> define/.)) . ((886 . 2))) ((def ((lib "formica/arity.rkt") inherit-arity)) . ((3682 . 3))) ((form (#<path:/home/serpol/dev/racket/formica/main.rkt> define//.)) . ((1230 . 2))) ((def ((lib "formica/tools.rkt") greedy)) . ((2015 . 3))) ((def ((lib "formica/tags.rkt") check-tag)) . ((2956 . 4))) ((def ((lib "formica/formal.rkt") formal-function?)) . ((1542 . 3))) ((def ((lib "formica/tools.rkt") curryr)) . ((2514 . 7))) ((def ((lib "formica/arity.rkt") variadic?)) . ((3338 . 3))) ((form (#<path:/home/serpol/dev/racket/formica/main.rkt> /.)) . ((858 . 2))) ((def ((lib "formica/tools.rkt") any-args)) . ((2467 . 3))) ((def ((lib "formica/arity.rkt") polyadic?)) . ((3285 . 3))) ((def ((lib "formica/partial-app.rkt") apply*)) . ((0 . 4))) ((def ((lib "formica/tools.rkt") function?)) . ((1784 . 3))) ((form ((lib "formica/formal.rkt") define-formal)) . ((1269 . 8))) ((def ((lib "formica/tools.rkt") curried?)) . ((2681 . 3))) ((def ((lib "formica/tags.rkt") set-tag*)) . ((3129 . 4))) ((form (#<path:/home/serpol/dev/racket/formica/main.rkt> //.)) . ((1201 . 2))) ((def ((lib "formica/formal.rkt") hold)) . ((1409 . 4))) ((def ((lib "formica/formal.rkt") n/f-pair?)) . ((1667 . 5))) ((def ((lib "formica/arity.rkt") fixed-arity?)) . ((3229 . 3))) ((form (#<path:/home/serpol/dev/racket/formica/main.rkt> rewrite-all)) . ((821 . 2))) ((def ((lib "formica/arity.rkt") nullary?)) . ((3391 . 3))) ((def ((lib "formica/tags.rkt") set-tag)) . ((3026 . 4))) ((def ((lib "formica/tools.rkt") composition)) . ((1960 . 3))) ((def ((lib "formica/arity.rkt") unary?)) . ((3443 . 3))) ((def ((lib "formica/arity.rkt") max-arity)) . ((3609 . 3))) ((def ((lib "formica/tools.rkt") andf)) . ((2228 . 4))) ((def ((lib "formica/tags.rkt") tag?)) . ((2909 . 3))) ((def ((lib "formica/tools.rkt") fif)) . ((2152 . 5))))
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
(polyadic? f) -> boolean?
  f : Any
procedure
(variadic? f) -> boolean?
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
