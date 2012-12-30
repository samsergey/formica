#lang setup/infotab

(define scribblings '(("doc/eng/formica.scrbl" (multi-page) (language 50))))

(require string-constants)
(define name "Formica") 
(define compile-omit-paths '("variants"
                             "monad/examples" 
                             "rewrite/examples"))
(define drscheme-language-modules '(("main.rkt" "formica")))

