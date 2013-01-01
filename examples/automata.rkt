#lang formica
(require rackunit)

;;;====================================================
;;; Definition for a recognising deterministic finite automata
;;;====================================================
(define (DFA start program)
  ; returns a function accepting input stream (list)
  (λ(input)
    (foldl program start input)))

;;----------------------------------------------------
;; Examples:
;;----------------------------------------------------

(define even/odd
  ;  Determines whether given binary number is even or odd.
  (DFA 'even
       (/. 0 'even --> 'even
           1 'even --> 'odd
           0 'odd --> 'even
           1 'odd --> 'odd)))

(test-case
 "simple DFA"
 (check-equal? (even/odd '(1 0 1))  'odd)
 (check-equal? (even/odd '(1 1 0))   'even))

(define div/3
  ;  Determines the remainder of division by 3 for a binary number.
  (compose
   (/. 's0 --> 0 's1 --> 1 's2 --> 2)
   (DFA 's0
        (/. 0 's0 --> 's0
            1 's0 --> 's1
            0 's1 --> 's2
            1 's1 --> 's0
            0 's2 --> 's1
            1 's2 --> 's2))))

(test-case
 "div/3 DFA"
 (check-equal? (div/3 '(0 1 1))  0)
 (check-equal? (div/3 '(1 0 0))  1)
 (check-equal? (div/3 '(1 0 1))  2))

;;====================================================
;;  Definition for a transdusing finite automata
;;====================================================
(define (TFA start program)
  ; returns a function accepting input stream (list)
  (λ(input)
    (foldr (/. el (cons S res) --> (append (program el S) res)) 
           (list start) 
           input)))

;;----------------------------------------------------
;; Examples:
;;----------------------------------------------------

(define add1
  ; adds 1 to a binary number
  (TFA 1
       (/. 0 0 --> '(0 0)
           0 1 --> '(0 1)
           1 0 --> '(0 1)
           1 1 --> '(1 0))))

(test-case
 "add1 TFA" 
 (check-equal? (add1 '(0 0 0))  '(0 0 0 1))
 (check-equal? (add1 '(0 0 1))  '(0 0 1 0))
 (check-equal? (add1 '(0 1 0))  '(0 0 1 1))
 (check-equal? (add1 '(1 1 1))  '(1 0 0 0))
 (check-equal? (add1 '())  '(1)))

(define complement
  ; Returns a complement for a binary number
  (compose 
   cdr
   (TFA 'A
        (/. 0 'A --> '(A 0)
            1 'A --> '(B 1)
            0 'B --> '(B 1)
            1 'B --> '(B 0)))))


(test-case
 "complement DFA" 
 (check-equal? (complement '(0 0 0))  '(0 0 0))
 (check-equal? (complement '(0 0 1))  '(1 1 1))
 (check-equal? (complement '(0 1 0))  '(1 1 0))
 (check-equal? (complement '(0 1 1))  '(1 0 1))
 (check-equal? (complement '(1 1 1))  '(0 0 1)))

(define edge
  ; an edge-detector for a stream of 0/1
  (compose
   cdr
   (TFA 'start
        (/.  0 'start --> '(s0 0)
             1 'start --> '(s1 0)
             0 's0 --> '(s0 0)
             1 's0 --> '(s1 1)
             0 's1 --> '(s0 1)
             1 's1 --> '(s1 0)))))

(test-case
 "edge-detecting TFA" 
 (check-equal? (edge '(0 0 0))  '(0 0 0))
 (check-equal?  (edge '(0 0 1 1 1 0))  '(0 1 0 0 1 0))
 (check-equal? (edge '(1 1 0 0 0 1 1 0))  '(0 1 0 0 1 0 1 0)))

;;====================================================
;; A Normal Markov automata (algoryphms)
;;====================================================
(define NMA
  (//.
   `(,a ___  * 0  ,b ___) --> `(,@a 0 * * ,@b) 
   `(,a ___   1   ,b ___) --> `(,@a  0 *  ,@b) 
   `(,a ___   0   ,b ___) --> `(,@a       ,@b)))

(test-case
 "edge-detecting TFA" 
 (check-equal? (NMA '(0))        '())
 (check-equal? (NMA '(1))        '(*))
 (check-equal? (NMA '(1 0))      '(* *))
 (check-equal? (NMA '(1 1))      '(* * *))
 (check-equal? (NMA '(0 1 1 1))  '(* * * * * * *)))
