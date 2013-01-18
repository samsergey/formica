#lang setup/infotab

(define scribblings '(("doc/rus/formica.scrbl" (multi-page) (getting-started 50))))

(require string-constants)
(define name "Formica 1.0") 
(define compile-omit-paths '("examples" "tests" "plt"))
(define drscheme-language-modules '(("main.rkt" "formica")))

