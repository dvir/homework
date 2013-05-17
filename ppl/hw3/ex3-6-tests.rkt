#lang racket

(require "ex3-6.rkt" "type-expression-adt.rkt" "utils.rkt")
(provide (all-defined-out))


(define (enumerate-tree-test) 
    (test (enumerate-tree$ (list 1 (list 2 (list 3 4)) 5) (lambda(x) x)) => '(1 2 3 4 5))
  )


(define (replace->and+-test) 
    (test (replace->and+ 'Boolean) => 'Boolean)
    (test (replace->and+ 1) => 1)
    (test (replace->and+ (make-proc-te (make-tuple-te (list 'Number)) (make-proc-te (make-tuple-te (list 'Number)) 'Number))) 
          => '(-> (+ Number) (=> (* Number) Number)))
    (test (replace->and+ (make-proc-te (make-tuple-te (list 'Number (make-proc-te (make-tuple-te (list 'Number)) 'Number) 'Number)) 'Number))
          => '(-> (+ Number (=> (* Number) Number) Number) Number))
  )

(run-tests 
     (enumerate-tree-test)
     (replace->and+-test)
)