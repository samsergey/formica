#lang formica
(require rackunit)

;;;=============================================================
;;; The definition of a functional type Tape, 
;;; representing infinite tape with O(1) operations: 
;;; put, get, shift-right and shift-left.
;;;=============================================================

(struct Tape (the-left-part      ; i-1 i-2 i-3 ...
              the-current-record ; i
              the-right-part) #:transparent)   ; i+1 i+2 i+3 ...

;; The tape in initial state
(define/. initial-tape 
  (cons h t) --> (Tape '() h t))

;; shift caret to the right
(define (snoc a b) (cons b a))
(define/. shift-right
  (Tape '() '() (cons h t)) --> (Tape '() h t)            ; left end
  (Tape  l x '())           --> (Tape (snoc l x) '() '()) ; right end
  (Tape  l x (cons h t))    --> (Tape (snoc l x) h t))    ; general case

;;  shift caret to the left
(define/. flip-tape 
  (Tape l x r) --> (Tape r x l))

(define shift-left 
  (compose flip-tape shift-right flip-tape))

;; access to the current record on the tape
(define/. get 
  (Tape _ v _) --> v)

(define/. put 
  '() t --> t            
  v (Tape l _ r) --> (Tape l v r))

;; List representation of the tape (≤ O(n)).
;; A tape is shown as (... a b c (d) e f g ...)
;; where (d) marks the current position of the caret.
(define (revappend a b) 
  (foldl cons b a))
(define/. show-tape
  (Tape '() '() '()) --> '() 
  (Tape l '() r) --> (revappend l (cons '() r)) 
  (Tape l v r) --> (revappend l (cons (list v) r)))


;;;=============================================================
;;; The Turing Machine interpreter
;;;
;;; The machine is defined by a program -- a set of rules:
;;;
;;;                 '(S1 v1) --> '(S2 v2 d)
;;;
;;; where S1 and v1 -- are current state and record on the tape,
;;;       S2 and v2 -- are new state and record,
;;;       d ∈ {'r 'l 'p} -- is a caret shift direction.
;;;=============================================================

;; interpretation of output triple for a given tape
(define/. interprete
  (list S v 'r) tape --> (list S (shift-right (put v tape))) 
  (list S v 'l) tape --> (list S (shift-left  (put v tape))) 
  (list S v 'p) tape --> (list S              (put v tape))  
  (list S _)    tape --> (list S                     tape))

;; Running the program.
;; The initial state is set to 'Start.
;; The initial tape is given as a list of records.
;; The initial position is the leftmost symbol of initial record.
(define (run-turing prog t0 verbose)
  ((rewrite-repeated
    `(,S ,T) --> (begin
                   (when verbose (printf "~a\t~a\n" S (show-tape T)))
                   (interprete (prog `(,S ,(get T))) T)))
   (list 'Start (initial-tape t0))))

;; A syntax for definition of a Turing-Machines.
;; Transforms to a function which accepts a list of initial 
;; tape records as input and returns the tape after stopping. 
(define-syntax-rule (Turing-Machine prog ...)
  (λ (l (verbose #t))
    (when verbose (displayln "STATE\tTAPE"))
    ((/. (list _ t) --> (flatten (show-tape t))) 
     (run-turing (rewrite prog ...) l verbose))))

;;;=============================================================
;;; Examples
;;;=============================================================

(define ADD1
  ; adds 1 to a binary number
  (Turing-Machine
   '(Start 1) --> '(Start 1 r) 
   '(Start 0) --> '(Start 0 r) 
   '(Start ()) --> '(Add () l) 
   '(Add 0) --> '(End 1 p) 
   '(Add 1) --> '(Add 0 l) 
   '(Add ()) --> '(End 1 p)))

; to see this machine in work run the commands
; (ADD1 '(1 0 1 1 1 1))
; (ADD1 (ADD1 '(1 1 0)))

(test-case
 "Turing machene tests"
 (check-equal? (ADD1 '(1 0 1) #f)  '(1 1 0))
 (check-equal? (ADD1 '(0)  #f)      '(1))
 (check-equal? (ADD1 '(1)  #f)      '(1 0))
 (check-equal? (ADD1 '(1 1 1)  #f)  '(1 0 0 0)))

; Here we cheat and use patterns to save lines of code.
; In classical Turing machine program we need to consider
; all possible symbols for all states.
(define ADDER
  ; adds unary numbers
  (Turing-Machine
   '(Start 1) --> '(Drag * r) 
   '(Start +) --> '(Start + r) 
   '(Start =) --> '(End = p) 
   '(Drag ()) --> '(Return 1 l) 
   `(Drag ,x) --> `(Drag ,x r) 
   '(Return *) --> '(Start 1 r) 
   `(Return ,x) --> `(Return ,x l)))

; to see this machine in work run the command
; (ADDER '(1 1 + 1 1 1 =))

(test-case
 "Turing machene tests"
 (check-equal? (ADDER '(1 + 1 =) #f)         '(1 + 1 = 1 1))
 (check-equal? (ADDER '(1 1 + 1 1 1 =)   #f)   '(1 1 + 1 1 1 = 1 1 1 1 1))
 (check-equal? (ADDER '(1 1 + 1 + 1 1 =)  #f)  '(1 1 + 1 + 1 1 = 1 1 1 1 1))
 (check-equal? (ADDER '(1 1 =)  #f)           '(1 1 = 1 1))
 
 (check-equal? (initial-tape '(1 2))  (Tape '() 1 '(2))))