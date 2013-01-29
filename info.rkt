#lang setup/infotab

(define scribblings '(("doc/eng/formica.scrbl" (multi-page) (language 50))))

(require string-constants)
(define name "Formica 1.0") 
(define compile-omit-paths '("examples" "tests" "plt"))
(define drscheme-language-modules '(("main.rkt" "formica")))

