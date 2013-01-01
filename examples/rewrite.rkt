#lang formica
(require rackunit)

;;====================================================
;; простые примеры
;;====================================================
(test-case
 "rewrite : simple tests"
 ;; однократное переписывание
  (check-equal? ((/. 'a --> 'b 
      'b --> 'c
      'c --> 'd) '(a b c d))  '(b c d d))
 
   (check-equal? ((/. 'a --> 'b 
       'b --> 'c
       'c --> 'a) '(a b c d))  '(b c a d))
 
 ;; однократное переписывание для нескольких переменных
  (check-equal? ((/. 'a --> 'b 
      'b 1 --> 'c
      'c 1 2 --> 'a) 'a)  'b)
 
  (check-equal? ((/. 'a --> 'b 
      'b 1 --> 'c
      'c 1 2 --> 'a) 'b 1)  'c)
 
  (check-equal? ((/. 'a --> 'b 
      'b 1 --> 'c
      'c 1 2 --> 'a) 'x 'y 'z)  '(x y z))
 
 ;; поиск нормальной формы
  (check-equal? ((//. 'a --> 'b 
        'b --> 'c
        'c --> 'd) '(a b c d))  '(d d d d))
 
  (check-equal? ((//. 'a -->. 'b ; терминальное правило
       'b --> 'a
       'c --> 'a) '(a b c d))  '(b b b d)))

;;====================================================
;; определение рекурсивных и итеративных функций
;;====================================================
(define/. length
  (cons _ t) --> (+ 1 (length t))
  '() --> 0)

(define/. depth
  (? list? x) --> (+ 1 (apply max (map depth x)))
  _ --> 0)


(define (fib1 n)
  (case n
    [(1) 0]
    [(2) 1]
    [else (let loop ([a 0] [b 1] [i n])
            (if (= i 3) 
                (+ a b)
                (loop b (+ a b) (- i 1))))]))

(define fib2
  (rewrite
   1 --> 0
   2 --> 1
   n --> (fib2 0 1 n)
   a b 3 --> (+ a b)
   a b i --> (fib2 b (+ a b) (- i 1))))

(define fib3
  (rewrite-repeated
   1 -->. 0
   2 -->. 1
   a b 3 -->. (+ a b)
   a b i --> (values b (+ a b) (- i 1))
   n --> (values 0 1 n)))

(define (benchmark)
  (let ([n 100000])
    (= (time (fib1 n)) (time (fib2 n)) (time (fib3 n)))))

(test-case
 "rewrite: recursion"
 (check-equal? (fib1 5)  (fib2 5))
 (check-equal? (fib1 5)  (fib3 5)))

;;====================================================
;; проверка списка на палиндромность
;;====================================================
(define/. palindrom?
  (or '() (list _)) --> #t 
  (list x y ___ x)  --> (palindrom? y))

(test-case
 "rewrite: palindrom"
  (check-equal? (palindrom? '()) #t)
  (check-equal? (palindrom? '(a a)) #t)
  (check-equal? (palindrom? '(a b a)) #t)
  (check-equal? (palindrom? '(к а з а к)) #t)
  (check-equal? (palindrom? '(r e v o l v e r))  '(o l))
  (check-equal? (palindrom? '(а  р о з а  у п а л а  н а  л а п у  а з о р а)) #t))

;;====================================================
;; правила разложения для логарифмической функции
;;====================================================
(define ln-expand
  (rewrite-all-repeated
   `(ln (,x __1 * ,y __1)) --> `((ln ,x) + (ln ,y)) 
   `(ln (,x __1 / ,y __1)) --> `((ln ,x) - (ln ,y)) 
   `(ln (,x ^ ,n))         --> `(,n * (ln ,x)) 
   `(ln (,x))              --> `(ln ,x)))

(test-case
 "rewrite: log expand"
 (check-equal? (ln-expand '(ln(x * y)))            '((ln x) + (ln y)))
 (check-equal? (ln-expand '(ln(x / y)))            '((ln x) - (ln y)))
 (check-equal? (ln-expand '(ln(x * y / z)))        '((ln x) + ((ln y) - (ln z))))
 (check-equal? (ln-expand '(ln(x / (y * z))))      '((ln x) - ((ln y) + (ln z))))
 (check-equal? (ln-expand '(ln(x ^ 2 / (y * z))))  '((2 * (ln x)) - ((ln y) + (ln z))))
 (check-equal? (ln-expand '(ln(x + y)))            '(ln (x + y)))
 (check-equal? (ln-expand '(ln(8 * (x + y))))      '((ln 8) + (ln (x + y))))
 (check-equal? (ln-expand '(ln(ln(x ^ n))))        '((ln n) + (ln(ln x)))))

;;====================================================
;; Symbolic η- and β-reductions for λ-calculus
;;====================================================

(define reduce
  (//. `(λ (,x) (,f ,x)) --> f
       `((λ (,x) ,body) ,E) --> ((eval `(/. ',x --> ',E)) body)))

;(reduce '((λ(f)(λ(x)((f f) x))) g))

;(reduce '((λ(n)(λ(f)(λ(x)((n f) (f x))))) (λ(f)(λ(x)(f x)))))

;;====================================================
;; Hoare's quicksort
;;====================================================
;qsort [] []
;qsort [H : T] = qsort (filter (< H) T) ++ H ++ qsort (filter (>= H) T

(define (split x l)
  (foldl (/. y `(,l ,r) --> (? (< y x)) `(,(cons y l) ,r)
             y `(,l ,r) -->             `(,l ,(cons y r))) 
         '(() ()) l))

(define qsort
  (rewrite-repeated
   (cons x y) --> (values x (split x y))
   x `(,l ,r) -->. (append (qsort l) `(,x) (qsort r))))

(test-case
 "rewrite: qsort"
 (check-equal? (qsort '())  '())
 (check-equal? (qsort '(1 1))  '(1 1))
 (check-equal? (qsort '(2 4 1 3 2 6 9 2))  '(1 2 2 2 3 4 6 9)))

;;====================================================
;; Нахождение корней алгебраических уравнений
;;====================================================

;; нахождение квадратного корня методом Ньютона
(define (sqrt-n n)
  ((//. x  --> (/ (+ (* x x) n) 2. x)) 1))

(test-case
 "rewrite: sqrt-n"
 (check almost-equal? (sqrt-n 2) (sqrt 2))
 (check almost-equal? (sqrt-n .1) (sqrt .1))
 (check almost-equal? (sqrt-n 1e3) (sqrt 1e3)))

;; Метод бисекции
(define (bisection f)
  (rewrite-repeated
   
   a b --> (values a b (f a) (f b))
   
   
   _ _ fa fb -->. (? (> (* fa fb) 0)) #f
   
   
   a b _ _   -->. (? (almost-equal? a b)) a
   
   
   a b fa fb -->. (let* ([c (/ (+ a b) 2.)]
                         [fc (f c)])
                    (or ((bisection f) a c fa fc)
                        ((bisection f) c b fc fb)))))

(test-case
 "rewrite: bisection"
 (check almost-equal? ((bisection (λ(x)(- x 2))) 1 3) 2)
 (check almost-equal? ((bisection (λ(x)(- (sin x) .4))) 0 2) (asin 0.4)))

(check-equal? ((//.
                a 0 -->. a
                a b --> (values b (modulo a b))) 6 9)
              3)

(define (secant f)
  (fixed-point (λ(x y)
                 (let ([fx (f x)] [fy (f y)])
                   (values y (/ (- (* x fy) (* y fx))
                                (- fy fx)))))))
