#lang formica
(define-type Data Sym Nat '())
(define-type State Sym)
(define-type (Tape (list: Data ..) Data (list: Data ..)))

(define-type Shift 'l 'r 'p)
(define-type In (list: State Data))
(define-type Out
  (list: State Data Shift)
  In)



(:: tape ((list: Data ..) -> Tape?)
  (define/. tape
    (list h t ...) --> (Tape '() h t)))

(:: shift-right (Tape? -> Tape?)
  (define/. shift-right
    (Tape '() '() (cons h t)) --> (Tape '() h t)
    (Tape l x '()) --> (Tape (cons x l) '() '())
    (Tape l x (cons h t)) --> (Tape (cons x l) h t)))

(:: flip-tape (Tape? -> Tape?)
  (define/. flip-tape
    (Tape l x r) --> (Tape r x l)))

(:: shift-left (Tape? -> Tape?)
  (define shift-left
    (compose flip-tape shift-right flip-tape)))

(:: get (Tape? -> Data)
  (define/. get (Tape _ x _) --> x))

(:: put (Data Tape? -> Tape?)
  (define/. put x (Tape l _ r) --> (Tape l x r)))

(:: show (Tape? -> list?)
  (define/. show
    (Tape l x r) --> (append (reverse l) `((,x)) r)
    (Tape l '() r) --> (append (reverse l) '() r)))


(:: interprete (Out Tape? -> (list: State Tape?))
  (define/. interprete
    (list S x 'r) T --> (list S (shift-left  (put x T)))
    (list S x 'l) T --> (list S (shift-right (put x T)))
    (list S x 'p) T --> (list S              (put x T))
    (list S _)    T --> (list S                     T)))

(:: run ((In -> Out) -> ((list: State Tape?) -> (list: State Tape?)))
  (define (run prog)
    (//. `(,S ,T) --> (begin
                        (show-protocol S T)
                        (interprete (prog `(,S ,(get T))) T)))))

(define (show-protocol S T)
  (printf "~a \t ~a \n" S (show T)))



(define-syntax-rule (Turing-Machine rules ...)
  (λ (t) (second ((run (/. rules ...)) (list 'Start t)))))

(:: ADD1 (Tape? -> Tape?)
  (define ADD1
    (Turing-Machine
     '(Start 1) --> '(Start 1 l)
     '(Start 0) --> '(Start 0 l)
     '(Start ()) --> '(Adder () r)
     '(Adder 0) --> '(End 1 p)
     '(Adder 1) --> '(Adder 0 r)
     '(Adder ()) --> '(End 1 p))))

