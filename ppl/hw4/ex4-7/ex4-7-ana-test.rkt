#lang racket
(require "utils.rkt" "analyzer-core.rkt")

(run-tests
 (test (derive-analyze-eval '((lambda l (+ (car l) (car (cdr l)))) 2 3)) => 5)
 (derive-analyze-eval '(define length
                 (lambda (l)
                   (if (null? l) 0
                       (+ 1 (length (cdr l)))))))
 (test (derive-analyze-eval '((lambda l (length l)) 1 2 3 'a)) => 4))

