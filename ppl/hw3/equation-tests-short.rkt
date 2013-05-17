#lang racket
(require "equation-adt.rkt" "utils.rkt")
(provide (all-defined-out))



; atomic tests
(define (atomic) 
    (test (make-equations '+ '((+ var_0))) => '((var_0 (-> (* Number Number) Number)))))

; lambda tests
(define (lambda) 
    (test (make-equations '(lambda(x)(x 11)) '(((lambda (x) (x 11)) T_0) ((x 11) T_1) (11 T_3) (x T_2))) => 
          '((T_0 (-> (* T_2) T_1)) (T_2 (-> (* T_3) T_1)) (T_3 Number))) 
  )

; application tests
(define (application) 
    (test (make-equations '((lambda (x) (+ x 1)) (lambda() (+ 1 2))) 
'((((lambda (x) (+ x 1)) (lambda () (+ 1 2))) var_0)
  ((lambda () (+ 1 2)) var_6)
  ((+ 1 2) var_7)
  (2 var_10)
  (1 var_9)
  (+ var_8)
  ((lambda (x) (+ x 1)) var_1)
  ((+ x 1) var_2)
  (1 var_5)
  (x var_4)
  (+ var_3))) => 
  '((var_1 (-> (* var_6) var_0))
  (var_6 (-> (*) var_7))
  (var_8 (-> (* var_9 var_10) var_7))
  (var_10 Number)
  (var_9 Number)
  (var_8 (-> (* Number Number) Number))
  (var_1 (-> (* var_4) var_2))
  (var_8 (-> (* var_4 var_9) var_2))
  (var_9 Number)
  (var_8 (-> (* Number Number) Number))))
)
  
(run-tests 
     (atomic)
     (lambda)
     (application))
