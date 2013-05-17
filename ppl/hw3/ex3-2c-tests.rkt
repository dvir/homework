#lang racket

(require "ex3-2c.rkt" "utils.rkt" "asp.rkt")

(provide (all-defined-out))

(define (lazy-expression-tests) 
    (test (tagged-by? (make-proc-te (make-tuple-te (list 'Number)) 'Number) '->) => #t)
    (test ((get-content (make-proc-te (make-tuple-te (list 'Number)) 'Number)) (lambda (x y)  #t))  => #t)
    (test (proc-return-te (make-proc-te (make-tuple-te (list 'Number)) 'Number)) => 'Number))


(run-tests 
     (lazy-expression-tests)) 

