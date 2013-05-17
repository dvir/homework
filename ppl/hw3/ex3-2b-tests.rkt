#lang racket


(require "type-expression-adt.rkt" "utils.rkt")

(provide (all-defined-out))


(define (polymorphic?-tests) 
    (test (polymorphic? 'Number) => #f)
    (test (polymorphic? (make-proc-te (make-tuple-te (list 'Number)) 'Boolean)) => #f)
    (test (polymorphic? (make-proc-te (make-tuple-te (list 'Number)) 'S4)) => #t))


(run-tests 
     (polymorphic?-tests))