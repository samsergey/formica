#lang scribble/doc

@(require (for-label formica))

@(require
  scribble/manual
  scribble/eval)

@(define formica-eval 
   (let ([sandbox (make-base-eval)])
     (sandbox '(require formica))
     sandbox))


@title[#:style '(toc) #:date "2012" #:version "0.0.1"]{Formica: The "functionally-oriented" language.}

@author{Sergey B. Samoylenko}

@smaller{Reference guide.}

@defmodulelang[formica]{This guide describes functions and syntax forms defined in the @racket[formica] language.
         
         Formica is available as both a language level and a module that can be used in other languages.}

@include-section["intro.scrbl"]

@include-section["syntax.scrbl"]

@include-section["rewrite.scrbl"]

@include-section["formal.scrbl"]

@include-section["types.scrbl"]

@include-section["monads.scrbl"]

@include-section["utils.scrbl"]

@local-table-of-contents[]

@bibliography[ 
  @bib-entry[#:key "Baader99"
             #:title "Term Rewriting and All That"
             #:author "Baader, F. and Nipkow, T."
             #:is-book? #t
             #:date "1999"
             #:location "Cambridge University Press"
             #:url "http://books.google.com/books?id=N7BvXVUCQk8C"]
                           
  @bib-entry[#:key "Bezem03"
             #:title "Term rewriting systems"
             #:author "Bezem, M., Klop, J.W. and Vrijer, R."
             #:url "http://books.google.com/books?id=oe3QKzhFEBAC"
             #:date "2003"
             #:location "Cambridge tracts in theoretical computer science. Cambridge University Press"
             #:is-book? #t]
                           
  @bib-entry[#:key "Turchin89"
             #:title  "REFAL-5 Programming Guide and Reference Manual"
             #:author "Turchin, V. F."
             #:is-book? #t
             #:date "1989"
             #:location "The City College of New York, New England Publishing Co., Holyoke."]
                                                                                             
  @bib-entry[#:key "Surhone10"
             #:title  "Refal"
             #:author "Surhone, L.M., Timpledon, M.T. and Marseken, S.F."
             #:is-book? #t
             #:date "2010"
             #:location "VDM Verlag Dr. Mueller AG & Co. Kg"
             #:url "http://books.google.com/books?id=SH2rcQAACAAJ"]
                           
   @bib-entry[#:key "Wolfram"
             #:title  "Wolfram Mathematica"
             #:is-book? #f
             #:url "http://www.wolfram.com/mathematica/"]

   @bib-entry[#:key "Findler"
                    #:author "Findler, R. and M. Felleisen" 
                    #:title "Contracts for higher-order functions"
                    #:location "ACM SIGPLAN International Conference on Functional Programming"
                    #:date "2002"]
   
   @bib-entry[#:key "Krishnamurthi"
                    #:title "Relationally-Parametric Polymorphic Contracts"
                    #:author "Krishnamurthi, S., Guha, A. et.al."
                    #:date "2007"
                    #:is-book? #f
                    #:url "http://cs.brown.edu/~sk/Publications/Papers/Published/gmfk-rel-par-poly-cont/"]]