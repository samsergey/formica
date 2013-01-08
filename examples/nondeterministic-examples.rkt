#lang formica
(require rackunit)

;;;==========================================================
;;; Non-deterministic computations
;;;==========================================================
(using-monad List)
;;;==========================================================
;; Finding a path in a graph
;;;==========================================================
(define (joins? a b)
  (eq? (last (string->list a))
       (first (string->list b))))

(define (find-path)
  (collect (list w1 w2 w3 w4)
    [ w1 <- '("the" "that" "a") ]    
    [ w2 <- '("frog" "elephant" "thing" "turtle") ]
    (joins? w1 w2)
    [ w3 <- '("walked" "eats" "treaded" "grows") ]
    (joins? w2 w3)
    [ w4 <- '("slowly" "quickly" "salad") ]
    (joins? w3 w4)))

(check-equal?
 (find-path)
 '(("that" "thing" "grows" "slowly") 
   ("that" "thing" "grows" "salad") 
   ("that" "turtle" "eats" "slowly") 
   ("that" "turtle" "eats" "salad")))

;; verbouse version showing the backtracking
(define (find-path-v)
  (define tell (lift displayln))
  (do [ w1 <- '("the" "that" "a") ]
      (tell (list w1))
      [ w2 <- '("frog" "elephant" "thing" "turtle") ]
      (tell (list w1 w2))
      (guard (joins? w1 w2))
      [ w3 <- '("walked" "eats" "treaded" "grows") ]
      (tell (list w1 w2 w3)) 
      (guard (joins? w2 w3))
      [ w4 <- '("slowly" "quickly" "salad") ]
      (tell (list w1 w2 w3 w4)) 
      (guard (joins? w3 w4)) 
      (tell "success!") 
      (return (list w1 w2 w3 w4))))

;;;==========================================================
;;; (((1 ? 2) ? 3) ? 4) ? 5 = N 
;;;==========================================================
(define (express N) 
  (define n object-name)
  (collect  `((((1 ,(n op4) 2) ,(n op3) 3) ,(n op2) 4) ,(n op1) 5 = ,N)
    [(op1 op2 op3 op4) <<- (list + - * /)] 
    (different? op1 op2 op3 op4)
    (= N (op1 (op2 (op3 (op4 1 2) 3) 4) 5))))


(check-equal? (bind (range 1 20) >>= express)
              '(((((1 + 2) * 3) - 4) / 5 = 1) ((((1 / 2) + 3) * 4) - 5 = 9)))

;;;==========================================================
;;;  SEND + 
;;;  MORE = 
;;; MONEY
;;;==========================================================

(define (from-digits . lst)
  (foldl (λ(d res) (+ d (* 10 res))) 0 lst))

(define (SENDMORY)
  (do [ S <-: 9 ]
      [ E <-: 5 ] 
      [ N <-: 6 ] 
      [ (D M O R Y) <<- (range 0 9) ] 
      (guard (different? S E N D M O R Y))
      (guard (= (+ (from-digits   S E N D)
                   (from-digits   M O R E))
                (from-digits    M O N E Y)))
      (return (printf "~a~a~a~a + ~a~a~a~a = ~a~a~a~a~a\n" 
                      S E N D M O R E M O N E Y))))


;;;==========================================================
;;; N-queens problem
;;;==========================================================

(define (N-queens1 N)
  (define/. safe?
    _ '() _ --> #t
    x (cons c y) n --> (not (or (= x c)
                                (= (abs (- x c)) n)
                                (not (safe? x y (+ n 1))))))
  (define/. solution
    0 --> (return '())
    n --> (collect (cons x y) 
            [y <- (solution (- n 1))]
            [x <- (range 1 (+ N 1))]
            (safe? x y 1)))
  (solution N))

(check-equal? (N-queens1 4) '((3 1 4 2) (2 4 1 3)))

;; using foldM operator
(define (N-queens2 N)
  (define/. safe?
    _ '() _ --> #t
    x (cons c y) n --> (not (or (= x c)
                                (= (abs (- x c)) n)
                                (not (safe? x y (+ n 1))))))
  
  (define/. safe-add
    y _ --> (collect (cons x y)
              [x <- (range 1 (+ N 1))]
              (safe? x y 1)))
  
  (fold/m safe-add '() (range 1 (+ N 1))))

(check-equal? (N-queens2 4) '((3 1 4 2) (2 4 1 3)))

;Бейкер, Купер, Флетчер, Миллер и Смит живут на разных этажах пятиэтаж-
;ного дома. Бейкер живет не на верхнем этаже. Купер живет не на первом
;этаже. Флетчер не живет ни на верхнем, ни на нижнем этаже. Миллер жи-
;вет выше Купера. Смит живет не на соседнем с Флетчером этаже. Флетчер
;живет не на соседнем с Купером этаже. Кто где живет?

(define (multiple-dwelling)
  (collect (list (list 'baker b)
                 (list 'cooper c)
                 (list 'fletcher f)
                 (list 'miller m)
                 (list 'smith s))
    [ b <- (range 1 5) ]
    [ f <- (range 2 5) ] (different? b f)
    [ c <- (range 2 6) ] (different? b f c)
    [ s <- (range 1 6) ] (different? b f c s)
    [ m <- (range c 6) ] (different? b f c s m)
    (> (abs (- c f)) 1)
    (> (abs (- s f)) 1)))
    
(check-equal? (multiple-dwelling)
              '(((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))))


;Пять школьниц писали экзаменационную работу.
;Они решили, что каждая девочка должна написать домой 
;и при этом сделать одно верное и одно неверное утверждение. 
;Вот соответствующие выдержки из их писем:
;Бетти: «Китти была на экзамене второй, а я только третьей».
;Этель: «Я написала лучше всех. Второй была Джоан».
;Джоан: «Я была третьей, а бедная Этель последней».
;Китти: «Я оказалась второй. Мэри была только четвертой».
;Мэри:  «Я была четвертой. Первое место заняла Бетти».
;В каком порядке на самом деле расположились отметки девочек?

(define (xor a b) (if a (not b) b))
(define (girls)
  (do [ (k b e j m) <<- (range 1 6) ]
      (guard (xor (= k 2) (= b 3))) 
      (guard (xor (= e 1) (= j 2))) 
      (guard (xor (= j 3) (= e 5))) 
      (guard (xor (= k 2) (= m 4))) 
      (guard (xor (= m 4) (= b 1))) 
      (guard (different? k e j b m)) 
      (return (list (list 'Kathie k)
                    (list 'Etel e)
                    (list 'Joan j)
                    (list 'Betty b)
                    (list 'Mary m)))))

(check-equal? (girls)
              '(((Kathie 1) (Etel 5) (Joan 2) (Betty 3) (Mary 4))))

;;;==========================================================
;; beautifull example of powerset generator
;;;==========================================================
(define (powerset set) 
  (filter/m (λ(x) '(#t #f)) set))

(check-equal?
 (powerset '(a b c))
 '((a b c) (a b) (a c) (a) (b c) (b) (c) ()))