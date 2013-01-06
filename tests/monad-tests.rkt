#lang racket/base
(require "../monad.rkt"
         "../formal.rkt"
         "../rewrite.rkt"
         "../tools.rkt"
         "../types.rkt"
         rackunit)

(test-case
 "Monad Id."
 (define-formal f g)
 (check-equal? (using-monad) Id)
 (check-equal? (return 'x) 'x)
 (check-equal? (bind 'x >>= return) 'x)
 (check-equal? (bind 'x >>= f) (f 'x))
 (check-equal? (bind 'x >>= f >>= g) (g (f 'x)))
 (check-equal? (do [x <- 'x]
                   [y <- (f x)]
                   (return (g y))) 
               (let* ([x 'x]
                      [y (f x)])
                 (g y)))
 (check-equal? (do [(x y) <<- 'x]
                   [z <- (f x y)]
                   (return (g z))) (g (f 'x 'x)))
 (check-equal? (do [(cons x y) <- '(a . b)]
                   [z <- (f x y)]
                   (return (g z))) (g (f 'a 'b)))
 (check-equal? ((m-compose f g) 'x) ((compose f g) 'x)))


(test-case
 "Monad M (simple contaner)"
 (define-formal (m 1) f g)
 (define-monad M
   #:type m?
   #:return m
   #:bind (/. (m x) f --> (f x)))
 
 (using-monad M)
 (check-equal? (return 'x) (m 'x))
 (check-equal? (bind (m 'x) >>= return) (m 'x))
 (check-equal? (bind (m 'x) >>= (lift f)) (m (f 'x)))
 (check-equal? (bind (m 'x) >>= (lift f) >>= (lift g)) (m (g (f 'x))))
 (check-equal? (do [x <- (return 'x)]
                   [y <- ((lift f) x)]
                   (return (g y))) (m (g (f 'x))))
 (check-equal? (do [x <-: 'x]
                   [y <-: (f x)]
                   (return (g y))) (m (g (f 'x))))
 (check-equal? (do [(cons x y) <-: '(1 2)]
                   [z <-: (f x y)]
                   (return (g z))) (m (g (f 1 '(2)))))
 (check-equal? (do [((list x y) z) <<-: '(1 2)]
                   [t <-: (f x y)]
                   [w <-: (reverse z)]
                   (return (g t w))) (m (g (f 1 2) '(2 1))))
 (check-equal? (collect (g y)
                 [x <-: 'x]
                 [y <-: (f x)]) (m (g (f 'x))))
 (check-equal? ((m-compose (lift f) (lift g)) 'x) (m (f (g 'x)))))

(test-case
 "Monad MZ"
 (define-formal (m 1) f g)
 (define-type MZ? 'z m?)
 (define-monad-plus MZ
   #:type MZ?
   #:return (/. 'z --> 'z
                x --> (m x))
   #:bind (/. 'z f --> 'z
              (m x) f --> (f x))
   #:mzero 'z
   #:mplus (/. 'z x --> x
               x 'z --> x
               x y --> (($ +) x y)))
 
 (using-monad MZ)
 (check-equal? (return 'x) (m 'x))
 (check-equal? (return 'z) 'z)
 (check-equal? (bind (m 'x) >>= return) (m 'x))
 (check-equal? (bind 'z >>= return) 'z)
 (check-equal? (bind (return 'x) >>= (lift f)) (m (f 'x)))
 (check-exn exn:fail:contract? (λ () (bind 'x >>= (lift f)) (m (f 'x))))
 (check-equal? (bind 'z >>= (lift f)) 'z)
 (check-equal? (bind (return 'x) >>= (lift f) >>= (lift g)) (m (g (f 'x))))
 (check-equal? (bind 'z >>= (lift f) >>= (lift g)) 'z)
 (check-equal? (bind (return 'x) >>= (lift (/. 'x --> 'z)) >>= (lift g)) 'z)
 (check-equal? (bind (return 'x) >>= (guardf (const #t)) >>= (lift g)) (m (g 'x)))
 (check-equal? (bind (return 'x) >>= (guardf (const #f)) >>= (lift g)) 'z)
 (check-equal? (bind (return 3) >>= (guardf odd?) >>= (lift g)) (m (g 3)))
 (check-equal? (bind (return 3) >>= (guardf even?) >>= (lift g)) 'z)
 (check-equal? (do [x <-: 'x]
                   (guard #t)
                   (return (g x))) (m (g 'x)))
 (check-equal? (do [x <-: 3]
                   (guard #f)
                   (return (g x))) 'z)
 (check-equal? (collect (g y)
                 [x <-: 'x]
                 #t
                 [y <-: (f x)]) (m (g (f 'x))))
 (check-equal? (collect (g y)
                 [x <-: 'x]
                 #f
                 [y <-: (f x)]) 'z)
 (check-equal? ((m-compose (lift f) (lift g)) 'x) (m (f (g 'x))))
 (check-equal? (map (m-compose (lift f) (lift (/. 'a --> 'z))) '(a b)) '(z (m (f b))))
 (check-equal? (msum '(a b c)) '(+ a (+ b c)))
 (check-equal? (msum '(a z b z c)) '(+ a (+ b c)))
 (check-equal? (fold-m (lift f) 'x '(a b c)) '(m (f (f (f x a) b) c)))
 (check-equal? (filter-m (lift (const #t)) '(a b c)) (m '(a b c)))
 (check-equal? (filter-m (lift (const #f)) '(a b c)) (m '()))
 (check-equal? (filter-m (lift (/. 'b --> #f)) '(a b c d)) (m '(a c d)))
 (check-equal? (filter-m (lift odd?) '(1 2 3 4 5)) (m '(1 3 5)))
 (check-equal? (filter-m (lift odd?) '(1 2 3 4 5 6)) (m '(1 3 5)))
 (check-equal? (lift-m f (m 'x)) (m (f 'x)))
 (check-equal? (lift-m f (m 'x) (m 'y)) (m (f 'x 'y)))
 (check-equal? (lift-m f (m 'x) (m 'y) 'z) 'z)
 (check-exn exn:fail:contract? (λ () (lift-m f (m 'x) (m 'y) 't) 'z)))

