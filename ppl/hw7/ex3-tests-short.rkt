#lang racket

(require "ex3.rkt")
(require "utils.rkt")
(provide (all-defined-out) (all-from-out "utils.rkt"))



(define n1 (make-mbtree 1))
(define n2 (make-mbtree 2))


(define (ex3-a-tests)  
  (test (get-left (make-empty-mbtree)) => 'null)
  (test (get-value n1) => 1)
  (test (begin (set-value! n1 100) (get-value n1)) => 100)
  (test (mbtree? n1) => #t)
  (test (empty-mbtree? n1) => #f)
)

(set-left! n1 n2)

(define (ex3-b-test)
  (test (begin (set-leftmost-occurrence! n1 2 '*) (get-value n2)) => '*)
  )


(run-tests
 (ex3-a-tests)
 (ex3-b-test)
)


