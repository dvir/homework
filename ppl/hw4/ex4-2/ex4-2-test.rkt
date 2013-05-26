#lang racket
(require "asp.rkt")

(define id
  (lambda (expanded-exp)
    expanded-exp))

(run-tests
 (test 
  (cond->if$ '(cond ((> 3 2) 5) (else 1)) id) => '(if (> 3 2) 5 1))
 
 (test 
  (cond->if$ '(cond ((< 3 2) 5) (else 1)) id) => '(if (< 3 2) 5 1))
 
 (test 
  (cond->if$ '(cond ((= x 1) 10) ((= x 2) 20) ((= x 3) 30) (else 100)) id) 
  => '(if (= x 1) 10 (if (= x 2) 20 (if (= x 3) 30 100))))
 )
