#lang racket/base
;;______________________________________________________________
;;                   ______ 
;;                  (  //   _____ ____   .  __  __
;;                   ~//~ ((_)// // / / // ((_ ((_/_
;;                 (_//
;;..............................................................
;; Provides tagged procedures which could be 
;; identifyed by tags and are shown to be tagged.
;;==============================================================

(require racket/contract)

(provide 
 (contract-out
  (tagged? (any/c . -> . boolean?))
  (tag (tagged? . -> . symbol?))
  (check-tag (symbol? any/c . -> . boolean?))
  (set-tag ((symbol?) ((or/c #f symbol?)) . ->* . (procedure? . -> . tagged?)))
  (set-tag* ((symbol?) ((or/c #f symbol?)) . ->* . (procedure? . -> . procedure?)))))

;; ====================================================================================
;; Implementation
;; ====================================================================================

;; Gets a tag of the tagged procedure. 
;; tag :: tagged? -> symbol?
(struct tagged (f id) 
  #:property prop:procedure (struct-field-index f))

;; Checks a tag of the tagged procedure. 
;; tag :: symbol? any/c -> boolean?
(define (check-tag id f)
  (and (tagged? f)
       (eq? id (tag f))))

;; Gets a tag of the tagged procedure. 
;; tag :: tagged? -> symbol?
(define tag tagged-id)

;; Returns a tagged procedure which is equivalent to 
;; the given one with tagged name. 
;; set-tag :: symbol? [(or/c symbol? #f] -> (procedure? -> tagged?)
(define (set-tag id [f-id #f])
  (λ (f)
    (tagged ((set-tag* id f-id) f) id)))

;; Returns a procedure which is equivalent to 
;; the given one with tagged name.
;; It is impossible to get the tag of the result.
;; set-tag :: symbol? [(or/c symbol? #f] -> (procedure? -> procedure?)
(define (set-tag* id [f-id #f])
  (λ (f)
    (let* ([name (or f-id (object-name f) 'λ)]
           [id-name (if name
                        (string->symbol (format "~a:~a" id name))
                        id)])
      (procedure-rename f id-name))))
