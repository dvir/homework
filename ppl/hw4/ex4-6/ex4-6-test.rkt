#lang racket

(require "substitution-core.rkt" "asp.rkt")

(run-tests
 (test (derive-eval '(+ 4 5)) => '(value 9))
 (test (derive-eval '((lambda (x) 1) (/ 5 0))) => '(value 1))
 )