#lang racket
(require "interpreter-core.rkt" "asp.rkt" "env-ds.rkt")

(run-tests
 (derive-eval '(define f
                 (let ((a 1)
                       (b 2))
                   (lambda ()
                     (+ a b))))) 
 (test (get-all-vars
        (first-frame
         (procedure-environment 
          (lookup-variable-value 
           'f 
           the-global-environment)))) => '(a b))
 
 (derive-eval '(define f
                 (let ((a 1)
                       (b 2))
                   (lambda ()
                     (+ a b))))) 
 (test (get-all-vals
        (first-frame
         (procedure-environment 
          (lookup-variable-value 
           'f 
           the-global-environment)))) => '(1 2))
 )
