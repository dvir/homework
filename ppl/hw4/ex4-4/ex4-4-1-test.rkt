#lang racket
(require "asp.rkt" "interpreter-core.rkt" "env-ds.rkt")

(run-tests
 (derive-eval '(define foo
                 (lambda ()
                   1)))
 (derive-eval '(define goo
                 (lambda (x)
                   x)))

 (test (parameter-less?
        (lookup-variable-value 
         'foo
         the-global-environment)) => #t)
 
 (test (parameter-less?
        (lookup-variable-value 
         'goo 
         the-global-environment)) => #f) 
 )
