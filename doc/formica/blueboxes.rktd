1687
#hash(((def ((lib "formica/arity.rkt") variadic?)) . ((1374 . 3))) ((def ((lib "formica/arity.rkt") binary?)) . ((1529 . 3))) ((def ((lib "formica/tags.rkt") set-tag)) . ((1118 . 4))) ((def ((lib "formica/tools.rkt") andf)) . ((320 . 4))) ((def ((lib "formica/tools.rkt") const)) . ((108 . 3))) ((def ((lib "formica/tools.rkt") orf)) . ((385 . 4))) ((def ((lib "formica/tags.rkt") tag?)) . ((1001 . 3))) ((def ((lib "formica/tools.rkt") arg)) . ((66 . 3))) ((def ((lib "formica/arity.rkt") nullary?)) . ((1427 . 3))) ((def ((lib "formica/tools.rkt") curried?)) . ((773 . 3))) ((def ((lib "formica/arity.rkt") min-arity)) . ((1580 . 3))) ((def ((lib "formica/tools.rkt") any-args)) . ((559 . 3))) ((def ((lib "formica/tools.rkt") fixed-point)) . ((825 . 4))) ((def ((lib "formica/tools.rkt") id)) . ((49 . 2))) ((def ((lib "formica/arity.rkt") unary?)) . ((1479 . 3))) ((def ((lib "formica/tools.rkt") fif)) . ((244 . 5))) ((def ((lib "formica/tools.rkt") negated)) . ((152 . 3))) ((def ((lib "formica/tags.rkt") set-tag*)) . ((1221 . 4))) ((def ((lib "formica/tags.rkt") tagged?)) . ((951 . 3))) ((def ((lib "formica/tools.rkt") curry)) . ((606 . 7))) ((def ((lib "formica/tools.rkt") all-args)) . ((512 . 3))) ((def ((lib "formica/arity.rkt") polyadic?)) . ((1321 . 3))) ((def ((lib "formica/tools.rkt") function?)) . ((0 . 3))) ((def ((lib "formica/tools.rkt") fork)) . ((449 . 4))) ((def ((lib "formica/tags.rkt") check-tag)) . ((1048 . 4))) ((def ((lib "formica/arity.rkt") inherit-arity)) . ((1718 . 3))) ((def ((lib "formica/tools.rkt") curryr)) . ((606 . 7))) ((def ((lib "formica/tools.rkt") flipped)) . ((198 . 3))) ((def ((lib "formica/arity.rkt") max-arity)) . ((1645 . 3))))
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
