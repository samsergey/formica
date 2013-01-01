#lang formica
(require rackunit)

;;======================================================
;; Transformation of algebraic (arythmetic) expression
;; given in infix form to parenthesized prefix form.
;; This function uses singlefold rewriting with 
;; explicit recursion.
;;======================================================
(define/. infix->prefix
  ; The order of rewriting rules corresponds 
  ; to the precedence of operations.
  `(,x __1  +  ,y __1) --> (list '+ (infix->prefix x) (infix->prefix y))
  `(,x __1  -  ,y __1) --> (list '- (infix->prefix x) (infix->prefix y))
  `(,x __1  *  ,y __1) --> (list '* (infix->prefix x) (infix->prefix y))
  `(,x __1  /  ,y __1) --> (list '/ (infix->prefix x) (infix->prefix y))
  ; rewriting for general funtions
  `(,f ,x __1)         --> (list f (infix->prefix x))
  ; treating the expressions in parenthesis
  `(,x)                -->  (infix->prefix x))



;;======================================================
;; Transformation from parenthesized prefix form 
;; to postfix (reverse polish notation)
;;======================================================
(define ((nest f g) x y) (f (g x) y))

(define/. prefix->RPN
  (cons f x) --> (foldl (λ (el res) (append (prefix->RPN el) res)) (list f) x)
  x --> (list x))

(define infix->RPN (compose prefix->RPN infix->prefix))


;;======================================================
;; Calculation of expression, given in reverse polish notation
;;======================================================
(define (calculate-RPN expr)
  ; interpretation of operations
  (define/. read-stack
    (? number? n) s      --> (cons n s)
    '+ (list x y s ___)  --> (cons (+ x y) s) 
    '- (list x y s ___)  --> (cons (- x y) s) 
    '* (list x y s ___)  --> (cons (* x y) s) 
    '/ (list x y s ___)  --> (cons (/ x y) s) 
    x s --> (error "Expression contains unknown operation:" x))
  
  ; If after interpretation stack contains one entry, return it. 
   ((/. `(,s) --> s) 
    (reverse (foldl read-stack '() expr))))

(test-case
 "infix tests"
  (check-equal? (infix->prefix '(1))               1)
  (check-equal? (infix->prefix '(1 + 2 * x))       '(+ 1 (* 2 x)))
  (check-equal? (infix->prefix '((a + b) * c))     '(* (+ a b) c))
  (check-equal? (infix->prefix '(2 * sin(2 * x)))  '(* 2 (sin (* 2 x))))


  (check-equal? (infix->RPN '(1))               '(1))
  (check-equal? (infix->RPN '(1 + 2 * x))       '(x 2 * 1 +))
  (check-equal? (infix->RPN '((a + b) * c))     '(c b a + *))
  (check-equal? (infix->RPN '(2 * sin(2 * x)))  '(x 2 * sin 2 *))

  (check-equal? (calculate-RPN (infix->RPN '(1)))                   1)
  (check-equal? (calculate-RPN (infix->RPN '(1 + 2 * 3)))           7)
  (check-equal? (calculate-RPN (infix->RPN '((1 + 2) * 3)))         9)
  (check-equal? (calculate-RPN (infix->RPN '((1 + 2) * (3 - 4))))  -3)
  (check-equal? (calculate-RPN (infix->RPN '((1 + 2) / (3 - 4))))  -3))