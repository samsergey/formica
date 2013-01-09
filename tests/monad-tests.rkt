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
 (using-monad Id)
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
 (check-equal? ((compose/m f g) 'x) ((compose f g) 'x)))


(test-case
 "Monad M (simple contaner)"
 (define-formal m f g)
 (:: m-return (-> a (m: a))
   (define m-return m))
 (:: m-bind (-> (m: a) (-> a (m: b)) (m: b))
   (define m-bind (/. (m x) f --> (f x))))
 (define-monad M
   #:return m-return
   #:bind m-bind)
 
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
 (check-equal? (collect (g y) [x <-: 'x] [y <-: (f x)]) (m (g (f 'x))))
 (check-equal? ((compose/m (lift f) (lift g)) 'x) (m (f (g 'x))))
 (check-equal? (lift/m f (m 'x)) (m (f 'x)))
 (check-equal? (lift/m f (m 'x) (m 'y)) (m (f 'x 'y)))
 (check-exn exn:fail:contract? (λ () (lift/m f (m 'x) (m 'y) 't) 'z))
 (check-equal? (seq/m (map m '(a b c))) '(m (a b c)))
 (check-equal? (map/m (lift f) '(a b c)) '(m ((f a) (f b) (f c))))
 )

(test-case
 "Monad MZ"
 
 (define-formal m f g)
 (define-type MZ? 'z (m: Any))
 
 (:: mz-return (-> Any MZ?)
   (define/. mz-return 
     'z --> 'z
     x --> (m x)))
 
 (:: mz-bind (-> MZ? (-> Any MZ?) MZ?)
   (define/. mz-bind 
     'z f --> 'z
     (m x) f --> (f x)))
 
 (:: mz-mplus (-> MZ? MZ? MZ?)
   (define/. mz-mplus 
     'z x --> x
     x 'z --> x
     x y --> (m (($ +) x y))))
 
 (define-monad-plus MZ
   #:return mz-return
   #:bind mz-bind
   #:mplus mz-mplus
   #:mzero 'z)
 
 (using-monad MZ)
 (check-equal? (return 'x) (m 'x))
 (check-equal? (return 'z) 'z)
 (check-equal? (bind (m 'x) >>= return) (m 'x))
 (check-equal? (bind 'z >>= return) 'z)
 (check-equal? (bind (return 'x) >>= (lift f)) (m (f 'x)))
 (check-exn exn:fail:contract? (λ () (bind 'x >>= (lift f))))
 (check-equal? (bind 'z >>= (lift f)) 'z)
 (check-equal? (bind (return 'x) >>= (lift f) >>= (lift g)) (m (g (f 'x))))
 (check-equal? (bind 'z >>= (lift f) >>= (lift g)) 'z)
 (check-equal? (bind (return 'x) >>= (lift (/. 'x --> 'z)) >>= (lift g)) 'z)
 (check-equal? (bind (return 'x) >>= (guardf (const #t)) >>= (lift g)) (m (g 'x)))
 (check-equal? (bind (return 'x) >>= (guardf (const #f)) >>= (lift g)) 'z)
 (check-equal? (bind (return 3) >>= (guardf odd?) >>= (lift g)) (m (g 3)))
 (check-equal? (bind (return 3) >>= (guardf even?) >>= (lift g)) 'z)
 (check-equal? (do [x <-: 'x] (guard #t) (return (g x))) (m (g 'x)))
 (check-equal? (do [x <-: 3] (guard #f) (return (g x))) 'z)
 (check-equal? (collect (g y) [x <-: 'x] #t [y <-: (f x)]) (m (g (f 'x))))
 (check-equal? (collect (g y) [x <-: 'x] #f [y <-: (f x)]) 'z)
 (check-equal? ((compose/m (lift f) (lift g)) 'x) (m (f (g 'x))))
 (check-equal? (map (compose/m (lift f) (lift (/. 'a --> 'z))) '(a b)) '(z (m (f b))))
 (check-equal? (fold/m (lift f) 'x '(a b c)) '(m (f c (f b (f a x)))))
 (check-equal? (filter/m (lift (const #t)) '(a b c)) (m '(a b c)))
 (check-equal? (filter/m (lift (const #f)) '(a b c)) (m '()))
 (check-equal? (filter/m (lift (/. 'b --> #f)) '(a b c d)) (m '(a c d)))
 (check-equal? (filter/m (lift odd?) '(1 2 3 4 5)) (m '(1 3 5)))
 (check-equal? (filter/m (lift odd?) '(1 2 3 4 5 6)) (m '(1 3 5)))
 (check-equal? (map/m (lift f) '(a b c)) '(m ((f a) (f b) (f c))))
 (check-equal? (map/m return '(a b c)) '(m (a b c)))
 (check-equal? (map/m return '(a z c)) 'z)
 (check-equal? (seq/m '((m a) (m b) (m c))) '(m (a b c)))
 (check-equal? (seq/m '((m a) z (m c))) 'z)
 (check-equal? (sum/m '((m a) (m b) (m c))) '(m (+ (m a) (m (+ (m b) (m c))))))
 (check-equal? (sum/m '((m a) z (m c))) '(m (+ (m a) (m c))))
 (check-equal? (lift/m f (m 'x)) (m (f 'x)))
 (check-equal? (lift/m f (m 'x) (m 'y)) (m (f 'x 'y)))
 (check-equal? (lift/m f (m 'x) (m 'y) 'z) 'z)
 (check-exn exn:fail:contract? (λ () (lift/m f (m 'x) (m 'y) 't))))

(test-case
 "Monad List"
 (define-formal f g)
 (using-monad List)
 (check-equal? (return 'x) '(x))
 (check-equal? (bind '(x y z) >>= return) '(x y z))
 (check-equal? (bind '() >>= return) '())
 (check-equal? (bind (return 'x) >>= (lift f)) '((f x)))
 (check-exn exn:fail:contract? (λ () (bind 'x >>= (lift f))))
 (check-equal? (bind '() >>= (lift f)) '())
 (check-equal? (bind (return 'x) >>= (lift f) >>= (lift g)) '((g (f x))))
 (check-equal? (bind '() >>= (lift f) >>= (lift g)) '())
 (check-equal? (bind (return 'x) >>= (/. 'x --> '()) >>= (lift g)) '())
 (check-equal? (bind (return 'x) >>= (guardf (const #t)) >>= (lift g)) '((g x)))
 (check-equal? (bind (return 'x) >>= (guardf (const #f)) >>= (lift g)) '())
 (check-equal? (bind (return 3) >>= (guardf odd?) >>= (lift g)) '((g 3)))
 (check-equal? (bind (return 3) >>= (guardf even?) >>= (lift g)) '())
 (check-equal? (do [x <-: 'x] (guard #t) (return (g x))) '((g x)))
 (check-equal? (do [x <-: 3] (guard #f) (return (g x))) '())
 (check-equal? (collect (g y) [x <-: 'x] #t [y <-: (f x)]) '((g (f x))))
 (check-equal? (collect (g y) [x <-: 'x] #f [y <-: (f x)]) '())
 (check-equal? ((compose/m (lift f) (lift g)) 'x) '((f (g x))))
 (check-equal? (map (compose/m (lift f) (/. 'a --> '() x --> (list x))) '(a b)) '(() ((f b))))
 (check-equal? (fold/m (lift f) 'x '(a b c)) '((f c (f b (f a x)))))
 (check-equal? (filter/m (lift (const #t)) '(a b c)) '((a b c)))
 (check-equal? (filter/m (lift (const #f)) '(a b c)) '(()))
 (check-equal? (filter/m (lift (/. 'b --> #f)) '(a b c d)) '((a c d)))
 (check-equal? (filter/m (lift odd?) '(1 2 3 4 5)) '((1 3 5)))
 (check-equal? (filter/m (lift odd?) '(1 2 3 4 5 6)) '((1 3 5)))
 (check-equal? (filter/m (λ (x) (list #t #f)) '(1 2 3)) '((1 2 3) (1 2) (1 3) (1) (2 3) (2) (3) ()))
 (check-equal? (map/m (lift f) '(a b c)) '(((f a) (f b) (f c))))
 (check-equal? (map/m return '(a b c)) '((a b c)))
 (check-equal? (seq/m '((a) (b) (c))) '((a b c)))
 (check-equal? (seq/m '((a) () (c))) '())
 (check-equal? (sum/m '((a) (b) (c))) '(a b c))
 (check-equal? (sum/m '((a) () (c))) '(a c))
 (check-equal? (lift/m f '(x)) '((f x)))
 (check-equal? (lift/m f '(x) '(y)) '((f x y)))
 (check-equal? (lift/m f '(x) '(y) '()) '())
 (check-equal? (lift/m f '(x y) '(a b)) '((f x a) (f x b) (f y a) (f y b)))
 (check-equal? (lift/m f '(x y) '(a)) '((f x a) (f y a)))
 
 )

(require "../examples/List-examples.rkt")
(require "../examples/nondeterministic-examples.rkt")