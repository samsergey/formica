#lang racket/base
(require "../private/tools/patterns.rkt"
         "test-utils.rkt"
         racket/match
         racket/stream)

(test-case
 "incremental match"
 (check-equal? (match 4 [(+ 1 x) x]) 3)
 (check-equal? (match 4 [(+ 2 x) x]) 2)
 (check-equal? (match 4 [(+ x 2) x]) 2)
 (check-equal? (match 4 [(+ 2 x) x] [(+ 1 x) x]) 2)
 (check-equal? (match 0 [(+ 1 x) x]) -1)
 (check-equal? (match 'x [(+ 1 x) x] [x x]) 'x)
 (check-equal? (match '(2 3) [(list (+ 1 x) (+ -1 y)) (list x y)]) '(1 4)))

     
(test-case
 "sequence match"
 (check-equal? (match (stream 1 2 3) [(scons x y) x]) 1)
 (check-equal? (match (stream 1 2 3) [(scons x (scons y z)) y]) 2)
 (check-equal? (match (stream 1 2 3) [(scons x (scons y z)) (stream? z)]) #t)
 (check-equal? (match (stream 1 2) [(scons x (scons y z)) (stream? z)]) #t)
 (check-equal? (match (stream 1) 
                 [(scons x (scons y z)) (stream? z)]
                 [x 'fail]) 'fail)
 (check-equal? (match (stream 1 (/ 0) 3) [(scons x y) x]) 1)
 (check-equal? (match (in-naturals) [(scons x _) x]) 0)
 (check-equal? (match (stream '(1 2) 3) [(scons (cons x _) _) x]) 1))