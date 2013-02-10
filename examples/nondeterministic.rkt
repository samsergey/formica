#lang formica
(require rackunit)

;;;==========================================================
;;; Non-deterministic computations using monad List
;;;==========================================================
(using-monad List)

;;;==========================================================
;; Following two examples could be done with help of
;; Racket's  for*/list form
;;;==========================================================
(test-case 
 "Finding a path in a graph"
 
 (define (joins? a b)
   (eq? (last (string->list a)) (first (string->list b))))
 
 (define (find-path)
   (collect (list w1 w2 w3 w4)
     [ w1 <- '("the" "that" "a") ]    
     [ w2 <- '("frog" "elephant" "thing" "turtle") ] (joins? w1 w2)
     [ w3 <- '("walked" "eats" "treaded" "grows") ]  (joins? w2 w3)
     [ w4 <- '("slowly" "quickly" "salad") ] (joins? w3 w4)))
 
 (check-equal?
  (find-path)
  '(("that" "thing" "grows" "slowly") 
    ("that" "thing" "grows" "salad") 
    ("that" "turtle" "eats" "slowly") 
    ("that" "turtle" "eats" "salad"))))

;;---------------------------------------------------------------------

(test-case
 "(((1 ? 2) ? 3) ? 4) ? 5 = N "
 
 (define (express N) 
   (define n object-name)
   (collect  `((((1 ,(n op4) 2) ,(n op3) 3) ,(n op2) 4) ,(n op1) 5 = ,N)
     [ ops <-: (list + - * /) ]
     [ (op1 op2) <<- ops ] (different? op1 op2)
     [ op3        <- ops ] (different? op1 op2 op3)
     [ op4        <- ops ] (different? op1 op2 op3 op4)
     (= N (op1 (op2 (op3 (op4 1 2) 3) 4) 5))))
 
 (check-equal? (express 9) '(((((1 / 2) + 3) * 4) - 5 = 9)))
 (check-equal? (bind (range 20) >>= express)
               '(((((1 + 2) - 3) / 4) * 5 = 0) 
                 ((((1 + 2) - 3) * 4) / 5 = 0) 
                 ((((1 + 2) * 3) - 4) / 5 = 1) 
                 ((((1 / 2) + 3) * 4) - 5 = 9))))


;;;==========================================================
;;; Following examples utilize different monadic functions
;;;==========================================================
;; The queen's positions are coded as a list, where each element
;; gives the row number, and the postion of an element corresponds
;; to column number.

(define (N-queens2 N)
  ; the safety predicate for a queen position (i j) in a given configuration
  (define/c (safe? i j)
    (/. '() --> #t
        (cons c cs) --> (and (not (= i c)) ; on the same row in the previous column
                             (not (= (abs (- i c)) j)) ; on the diagonal
                             (safe? i (+ j 1) cs)))) ; same for other columns
  
  ; adds new safe cell to existing configuration
  (define (safe-add _ conf)
    (collect (cons x conf) [x <- (range 1 (+ N 1))] (safe? x 1 conf)))
  
  ; look for all possible solutions
  (fold/m safe-add '() (range 1 (+ N 1)))) 

(test-case
 "N-queens problem"
 
 (check-equal? (N-queens2 4) '((3 1 4 2) (2 4 1 3))))

;;---------------------------------------------------------------------
;; An elegant powerset generator.
;; Here the filtering function may return either #t or #f, 
;; list of all possibilities gives a powerset of a given set.
(test-case
 "powerset"
 (define powerset (filter/m (const '(#t #f))))
 
 (check-equal? (powerset '(a b c)) '((a b c) (a b) (a c) (a) (b c) (b) (c) ()))
 (check-equal? (powerset '(a)) '((a) ())))

;;---------------------------------------------------------------------
;; This example shows how monadic composition allows to combine 
;; non-deterministic functions
(test-case
 "computation of complex roots"
 ;; c-root returns a list of all complex roots of number x for given power n
 (define (c-root n x)
   (collect (make-polar ρ φ)
     [ρ <-: (expt (magnitude x) (/ 1 n))]
     [k <- (range 0 n)]
     [φ <-: (+ (angle x) (* 2 pi k (/ 1 n)))]))
 
 ;; set the tolerance for tests
 (tolerance 1e-14)
 
 ;; complex root returns a list
 (check almost-equal? (c-root 3 8) '(2 -1+1.7320508075688774i -1-1.7320508075688767i))
 
 ;; composition of functions, representing a root of six'th power.
 (check almost-equal? 
        (bind '(64) >>= (c-root 2) >>= (c-root 3))
        '(2
          -1+1.7320508075688774i
          -1-1.7320508075688767i
          -2
          1-1.732050807568878i
          1+1.7320508075688767i))
 ;; another representation for a root of six'th power.
 (check almost-equal? 
        ((compose/m (c-root 2) (c-root 3)) 64)
        '(2 -2
            -1+1.7320508075688774i
            1-1.732050807568878i
            -1-1.7320508075688767i
            1+1.7320508075688767i)))

;;---------------------------------------------------------------------
;; The simple non-deterministic parser.
;; The example is adopted from the HaskelWiki 
;; <http://www.haskell.org/haskellwiki/All_About_Monads>
(test-case
 "non-deterministic parser"
 ; tokens
 (define-formal Hex Dec Word Bin)
 ;; we can get three different types of terms
 (define-type Parsed (Hex: Int) (Dec: Int) (Word: Str) (Bin: Int))
 
 ; predicates for types of characters
 (define hex-char? (regexp-match? #rx"[0-9a-f]"))
 (define dec-char? (regexp-match? #rx"[0-9]"))
 (define alph-char? (regexp-match? #rx"[a-z]"))
 (define bin-char? (regexp-match? #rx"[01]"))
 
 ;; attempts to add a character to the parsed representation of a hex digit
 (:: parse-hex (Str Parsed -> (list: Parsed ..))
   (define/. parse-hex
     (? hex-char? c) (Hex d) --> (return (Hex (+ (* 16 d) (string->number c 16))))
     _                _      --> mzero))
 
 ;; attempts to add a character to the parsed representation of a decimal digit
 (:: parse-dec (Str Parsed -> (list: Parsed ..))
   (define/. parse-dec
     (? dec-char? c) (Dec d) --> (return (Dec (+ (* 10 d) (string->number c))))
     _               _       --> mzero))
 
 ;; attempts to add a character to the parsed representation of a word
 (:: parse-word (Str Parsed -> (list: Parsed ..))
   (define/. parse-word
     (? alph-char? c) (Word w) --> (return (Word (string-append w c)))
     _                _        --> mzero))
 
 ;; attempts to add a character to the parsed representation of a hex digit
 (:: parse-bin (Str Parsed -> (list: Parsed ..))
   (define/. parse-bin
     (? bin-char? c) (Bin d) --> (return (Bin (+ (* 2 d) (string->number c 2))))
     _                _      --> mzero))
 
 ;; tries to parse the digit as a hex value, a decimal value and a word
 ;; the result is a list of possible parses
 (:: parse-char (Str Parsed -> (list: Parsed ..))
   (define (parse-char c p)
     (mplus (parse-hex c p) (parse-dec c p) (parse-word c p) (parse-bin c p))))
 
 ;; parse an entire String and return a list of the possible parsed values
 (:: parse (Str -> (list: Parsed ..))
   (define (parse str)
     (do [s <-: (map string (string->list str))]
         [init <- '((Hex 0) (Dec 0) (Word "") (Bin 0))]
         (fold/m parse-char init s))))
 
 (check-equal? (parse "12") '((Hex 18) (Dec 12)))
 (check-equal? (parse "dead") '((Hex 57005) (Word "dead")))
 (check-equal? (parse "1a") '((Hex 26)))
 (check-equal? (parse "abx") '((Word "abx")))
 (check-equal? (parse "a1bx") '()))