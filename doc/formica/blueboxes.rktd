3006
#hash(((def ((lib "formica/arity.rkt") variadic?)) . ((2652 . 3))) ((form ((lib "formica/formal.rkt") formal)) . ((1059 . 2))) ((def ((lib "formica/formal.rkt") formal-function?)) . ((956 . 3))) ((form (#<path:/home/serpol/dev/racket/formica/main.rkt> define/.)) . ((300 . 2))) ((def ((lib "formica/tools.rkt") function?)) . ((1198 . 3))) ((def ((lib "formica/arity.rkt") unary?)) . ((2757 . 3))) ((def ((lib "formica/tools.rkt") const)) . ((1330 . 3))) ((def ((lib "formica/tools.rkt") orf)) . ((1607 . 4))) ((form (#<path:/home/serpol/dev/racket/formica/main.rkt> rewrite)) . ((67 . 6))) ((def ((lib "formica/partial-app.rkt") apply*)) . ((0 . 4))) ((def ((lib "formica/tools.rkt") any-args)) . ((1781 . 3))) ((def ((lib "formica/tools.rkt") all-args)) . ((1734 . 3))) ((def ((lib "formica/arity.rkt") inherit-arity)) . ((2996 . 3))) ((form (#<path:/home/serpol/dev/racket/formica/main.rkt> rewrite-all-repeated)) . ((569 . 2))) ((def ((lib "formica/tools.rkt") andf)) . ((1542 . 4))) ((def ((lib "formica/tags.rkt") tag?)) . ((2223 . 3))) ((def ((lib "formica/arity.rkt") fixed-arity?)) . ((2543 . 3))) ((def ((lib "formica/tools.rkt") curry)) . ((1828 . 7))) ((def ((lib "formica/tools.rkt") negated)) . ((1374 . 3))) ((def ((lib "formica/tools.rkt") curried?)) . ((1995 . 3))) ((def ((lib "formica/tags.rkt") set-tag)) . ((2340 . 4))) ((def ((lib "formica/arity.rkt") min-arity)) . ((2858 . 3))) ((form ((lib "formica/formal.rkt") define-formal)) . ((683 . 8))) ((def ((lib "formica/tags.rkt") tagged?)) . ((2173 . 3))) ((form (#<path:/home/serpol/dev/racket/formica/main.rkt> /.)) . ((272 . 2))) ((def ((lib "formica/tools.rkt") arg)) . ((1288 . 3))) ((def ((lib "formica/tags.rkt") set-tag*)) . ((2443 . 4))) ((form (#<path:/home/serpol/dev/racket/formica/main.rkt> define//.)) . ((644 . 2))) ((def ((lib "formica/arity.rkt") binary?)) . ((2807 . 3))) ((def ((lib "formica/formal.rkt") hold)) . ((823 . 4))) ((def ((lib "formica/arity.rkt") polyadic?)) . ((2599 . 3))) ((def ((lib "formica/tools.rkt") flipped)) . ((1420 . 3))) ((form ((lib "formica/formal.rkt") formal-out)) . ((1169 . 2))) ((def ((lib "formica/arity.rkt") nullary?)) . ((2705 . 3))) ((def ((lib "formica/tools.rkt") fork)) . ((1671 . 4))) ((form (#<path:/home/serpol/dev/racket/formica/main.rkt> rewrite-all)) . ((235 . 2))) ((def ((lib "formica/tools.rkt") curryr)) . ((1828 . 7))) ((def ((lib "formica/tools.rkt") id)) . ((1247 . 3))) ((def ((lib "formica/formal.rkt") n/f-pair?)) . ((1081 . 5))) ((def ((lib "formica/arity.rkt") max-arity)) . ((2923 . 3))) ((def ((lib "formica/tools.rkt") fif)) . ((1466 . 5))) ((form (#<path:/home/serpol/dev/racket/formica/main.rkt> //.)) . ((615 . 2))) ((def ((lib "formica/tools.rkt") fixed-point)) . ((2047 . 4))) ((def ((lib "formica/tags.rkt") check-tag)) . ((2270 . 4))) ((def ((lib "formica/formal.rkt") n/f-list?)) . ((1081 . 5))) ((def ((lib "formica/formal.rkt") formal?)) . ((1012 . 3))) ((form (#<path:/home/serpol/dev/racket/formica/main.rkt> rewrite-repeated)) . ((338 . 9))))
procedure
(apply* f v ...) -> Any
  f : Fun
  v : Any
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
