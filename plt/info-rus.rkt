#lang setup/infotab

(define scribblings '(("doc/rus/formica.scrbl" (multi-page) (getting-started 50))))

(require string-constants)
(define name "Formica1.0") 
(define compile-omit-paths '("tests/all-tests.rkt" "plt"))
(define drscheme-language-modules '(("main.rkt" "formica")))

