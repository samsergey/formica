#lang racket/base
(require "../private/tools/tags.rkt"
         "test-utils.rkt")

(define tagged-cons ((set-tag 'tag) cons))
(define tagged-cons* ((set-tag* 'tag) cons))

(test-case 
 "tag testing"
  (check-equal? (tag tagged-cons) 'tag "Right tag")
  (check-exn exn:fail:contract? (λ () (tag 1)) "Not a procedure.")
  (check-exn exn:fail:contract? (λ () (tag cons)) "Not a tagged procedure.")
  (check-exn exn:fail:contract? (λ () (tag tagged-cons*)) "Not a tagged object."))

(test-case 
 "check-tag testing"
  (check-true (check-tag 'tag tagged-cons) "Right tag")
  (check-true (not (check-tag 'abc tagged-cons)) "Wrong tag")
  (check-true (not (check-tag 'tag cons)) "Not tagged procedure")
  (check-true (not (check-tag 'tag tagged-cons*)) "Not a tagged object.")
  (check-true (not (check-tag 'tag 1)) "Not a procedure"))

(test-case
 "set-tag testing"
  (check-equal? (tagged-cons 1 2)  (cons 1 2) "Functional equivalence")
  (check-equal? (procedure-arity tagged-cons) (procedure-arity cons) "Arity equivalence")
  (check-equal? (object-name tagged-cons) 'tag:cons "Naming")
  (check-true (procedure? tagged-cons) "Tagged procedure is a procedure.")
  (check-true (tagged? tagged-cons) "Tagged procedure is tagged."))

(test-case
 "set-tag* testing"
  (check-equal? (tagged-cons* 1 2)  (cons 1 2) "Functional equivalence")
  (check-equal? (procedure-arity tagged-cons*) (procedure-arity cons) "Arity equivalence")
  (check-equal? (object-name tagged-cons*) 'tag:cons "Naming")
  (check-true (procedure? tagged-cons) "Is tagged procedure a procedure?")
  (check-true (not (tagged? tagged-cons*)) "Not tagged object."))