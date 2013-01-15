#lang setup/infotab

(define scribblings '(("doc/eng/formica.scrbl" (multi-page) (language 50))))

(require string-constants)
(define name "Formica1.0") 
(define compile-omit-paths '("tests/all-tests.rkt"))
(define drscheme-language-modules '(("main.rkt" "formica")))

