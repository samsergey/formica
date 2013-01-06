#lang racket/base
(require racket/list
         racket/math
         racket/promise
         racket/string
         "tools.rkt"
         "regular-app/formal.rkt"
         "rewrite.rkt"
         "tacit.rkt"
         "memoize.rkt"
         "types.rkt"
         "ordering.rkt")
(provide 
 (except-out 
  (all-from-out racket/base
                racket/list
                racket/math
                racket/promise
                racket/string
                "tools.rkt"
                "regular-app/formal.rkt"
                "rewrite.rkt"
                "tacit.rkt"
                "memoize.rkt"
                "types.rkt"
                "ordering.rkt")
  procedure? lazy delay)
 (rename-out [procedure? function?]
             [lazy delay]))