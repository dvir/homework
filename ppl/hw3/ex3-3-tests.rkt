#lang racket


(require "asp.rkt" "ex3-3.rkt" "utils.rkt")

(provide (all-defined-out))


(define (equation-tests) 
    (test (tagged-data? (make-equation-from-tes 'T 'Number)) => #t)
    (test (tagged-data? (make-empty-equation)) => #t) 
    (test (get-right (make-equation-from-tes 'T 'Number)) => 'Number)
    (test (get-left (make-equation-from-tes 'T 'Number)) => 'T))


(run-tests 
     (equation-tests))
