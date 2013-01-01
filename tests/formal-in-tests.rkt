#lang racket/base
(require rackunit)

(require "formal-out-test.rkt")
;; to be done with 'formal-in' form
#;(require (except-in "formal-out-test.rkt" 
                    %a-provide-test%
                    %a-provide-test%?
                    %a-provide-test%:)
           (formal-in "formal-out-test.rkt"  %a-provide-test%))

(test-case
 "formal-out tests"
 (check-pred procedure? %a-provide-test%)
 (check-pred procedure? %a-provide-test%?)
 (check-pred procedure? (%a-provide-test%: 1 2))
 (check-pred procedure? %another-provide-test%)
 (check-pred procedure? %another-provide-test%?)
 (check-pred procedure? (%another-provide-test%: 1 2)))