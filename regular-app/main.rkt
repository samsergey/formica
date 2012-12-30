#lang racket/base
(require racket/list
         racket/contract
         racket/math
         racket/promise
         racket/string
         "../tools.rkt"
         "formal.rkt")
(provide 
 (except-out 
  (all-from-out racket/base
                racket/list
                racket/contract
                racket/math
                racket/promise
                racket/string
                "../tools.rkt"
                "formal.rkt")
  procedure? lazy delay)
 (rename-out [procedure? function?]
             [lazy delay]))