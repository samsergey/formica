#lang racket/base
(require "../private/types/ordering.rkt"
         "test-utils.rkt"
         racket/list)

(test-case
 "symbol<? tests"
 (check-true (symbol<? 'x 'y))
 (check-false (symbol<? 'y 'x))
 (check-false (symbol<? 'x 'x))
 (check-true (symbol<? 'x 'y 'z)))

(test-case
 "pair<? tests"
 (check-true (pair<? '(x) '(y)))
 (check-true (pair<? '(x a) '(y b)))
 (check-true (pair<? '(x b) '(y a)))
 (check-true (pair<? '(x (a 1)) '(x (a 2)))))

(test-case
 "ordered<? tests"
 (check-true (ordered?))
 (check-true (ordered? 1))
 (check-true (ordered? 'x 'y))
 (check-true (ordered? 1 "a" 'x))
 (let ([l '(#t #f 1 2.0 3  "ab" "abc" "bca" (1) (1 2) (1 (2 3)) (3 2) 'a 'b 'x 'y)])
   (for ([i 10])
     (check-equal? l (sort (shuffle l) ordered?)))))

(test-case
 "type-ordering and add-to-type-ordering tests"
 (parameterize ([type-ordering (list (cons odd? <)
                                     (cons even? >))])
   (check-true (ordered? '(1 3 5 7 9 8 6 4 2 0))))
 
 (parameterize ([type-ordering (type-ordering)])
   (add-to-type-ordering odd? #f >)
   (check-true (ordered? '(9 7 5 3 1 0 2 4 6 8))))
 
 (parameterize ([type-ordering (type-ordering)])
   (add-to-type-ordering odd? 'first >)
   (check-true (ordered? '(9 7 5 3 1 #t #f 0 2 4 6 8))))
 
 (parameterize ([type-ordering (type-ordering)])
   (add-to-type-ordering #f 'first)
   (check-true (ordered? '(#f #t 0 "a"))))
 
 (parameterize ([type-ordering (type-ordering)])
   (add-to-type-ordering #t #f)
   (check-true (ordered? '(#f #t 0 "a")))))
