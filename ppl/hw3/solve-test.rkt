#lang racket


(require "solve.rkt")
(require "equation-adt.rkt")
(require "substitution-adt.rkt")
(require "type-expression-adt.rkt")
(require "ass2.rkt")
(require "utils.rkt")
(provide (all-defined-out))



;Signature: equations-equal?(eq1 eq2)
;Type: Client view: [Equation*Equation -> Boolean]
;      Implementation view: [LIST -> Boolean]
;Purpose: COmpares two equations and return if both equal
;Tests: (equations-equal? '(T_1 T_2) '(T_2 T_1))  ==> #t
(define equations-equal?
  (lambda (eq1 eq2)
    (and (equation? eq1) (equation? eq2) (or (equal? eq1 eq2) (equal? (make-equation-from-tes (get-right eq1) (get-left eq1)) eq2)))
  ))


(define contains-equation? 
  (lambda (equation l) 
    (not (null? (filter (lambda(x) x) (map (lambda(x) (equations-equal? equation x)) l))))
    ))



(define equation-sets-equal? 
  (lambda(l1 l2)
       (and (null? (filter (lambda(x) (not x)) (map (lambda(x) (contains-equation? x l2)) l1)))
            (null? (filter (lambda(x) (not x)) (map (lambda(x) (contains-equation? x l1)) l2))))
    
  ))



(define one-of-two-equation-sets-equal? 
  (lambda(compared l1 l2)
       (or (and (null? (filter (lambda(x) (not x)) (map (lambda(x) (contains-equation? x l2)) compared)))
            (null? (filter (lambda(x) (not x)) (map (lambda(x) (contains-equation? x compared)) l2))))
           
           (and (null? (filter (lambda(x) (not x)) (map (lambda(x) (contains-equation? x l1)) compared)))
            (null? (filter (lambda(x) (not x)) (map (lambda(x) (contains-equation? x compared)) l1)))))
  ))



(define (infer-type-tests) 
    (test (infer-type '(lambda (x) (+ (+ x 1) (+ x 1)))) => '(-> (* Number) Number))
    (test (infer-type '((lambda(x)(x 11)) (lambda(y) y))) => 'Number)
    (test (infer-type '(lambda (x) (+ x 1))) => '(-> (* Number) Number)))


(define (split-equation-tests) 
    (test (one-of-two-equation-sets-equal? (split-equation (make-equation-from-tes '(-> (* T_3) (-> (* T_3) T_1)) '(-> (* T_3) (-> (* T_2) T_2)))) 
                                       '((T_1 T_2) (T_3 T_2) (T_3 T_3))
                                       '( ( (-> (* T_3) T_1) (-> (* T_2) T_2)) (T_3 T_3))) => #t)
    (test (one-of-two-equation-sets-equal? (split-equation (make-equation-from-tes '(-> (* T_3) Number) '(-> (* Boolean) T_2))) 
                                       '((Number T_2) (T_3 Boolean))
                                       '((Number T_2) (T_3 Boolean))) => #t)
    (test (one-of-two-equation-sets-equal? (split-equation (make-equation-from-tes '(-> (* T_3 Boolean Number) (-> (* T_3) Number)) 
                                                                                   '(-> (* T_3 T_2 Number) (-> (* Boolean) T_5))))
                                      '((Number T_5) (T_3 Boolean) (T_3 T_3) (Boolean T_2) (Number Number))
                                      '(((-> (* T_3) Number) (-> (* Boolean) T_5)) (T_3 T_3) (Boolean T_2) (Number Number))) => #t)) 




(run-tests 
     (split-equation-tests)
     (infer-type-tests))
