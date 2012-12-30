1752
#hash(((def ((lib "formica/tools.rkt") function?)) . ((67 . 3))) ((def ((lib "formica/tags.rkt") check-tag)) . ((1115 . 4))) ((def ((lib "formica/tools.rkt") fixed-point)) . ((892 . 4))) ((def ((lib "formica/tools.rkt") curried?)) . ((840 . 3))) ((def ((lib "formica/arity.rkt") polyadic?)) . ((1388 . 3))) ((def ((lib "formica/tools.rkt") andf)) . ((387 . 4))) ((def ((lib "formica/tools.rkt") curryr)) . ((673 . 7))) ((def ((lib "formica/tags.rkt") set-tag*)) . ((1288 . 4))) ((def ((lib "formica/tools.rkt") orf)) . ((452 . 4))) ((def ((lib "formica/tools.rkt") fork)) . ((516 . 4))) ((def ((lib "formica/tools.rkt") const)) . ((175 . 3))) ((def ((lib "formica/tools.rkt") any-args)) . ((626 . 3))) ((def ((lib "formica/arity.rkt") max-arity)) . ((1712 . 3))) ((def ((lib "formica/tools.rkt") fif)) . ((311 . 5))) ((def ((lib "formica/arity.rkt") binary?)) . ((1596 . 3))) ((def ((lib "formica/arity.rkt") inherit-arity)) . ((1785 . 3))) ((def ((lib "formica/tools.rkt") negated)) . ((219 . 3))) ((def ((lib "formica/partial-app.rkt") apply*)) . ((0 . 4))) ((def ((lib "formica/tools.rkt") all-args)) . ((579 . 3))) ((def ((lib "formica/tools.rkt") id)) . ((116 . 2))) ((def ((lib "formica/arity.rkt") variadic?)) . ((1441 . 3))) ((def ((lib "formica/tags.rkt") tagged?)) . ((1018 . 3))) ((def ((lib "formica/arity.rkt") min-arity)) . ((1647 . 3))) ((def ((lib "formica/tools.rkt") flipped)) . ((265 . 3))) ((def ((lib "formica/tags.rkt") tag?)) . ((1068 . 3))) ((def ((lib "formica/tools.rkt") arg)) . ((133 . 3))) ((def ((lib "formica/tools.rkt") curry)) . ((673 . 7))) ((def ((lib "formica/tags.rkt") set-tag)) . ((1185 . 4))) ((def ((lib "formica/arity.rkt") unary?)) . ((1546 . 3))) ((def ((lib "formica/arity.rkt") nullary?)) . ((1494 . 3))))
procedure
(apply* f v ...) -> Any
  f : Fun
  v : Any
procedure
(function? x) -> Bool
  x : Any
value
id : Fun
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
