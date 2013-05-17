#lang racket


(require "type-expression-adt.rkt" "substitution-adt.rkt" "utils.rkt")

(provide (all-defined-out))


(define (substitution-combination-tests) 
    (test (substitution-combination 
           (make-sub '(T1)
                     (list (make-tuple-te (list 'Number 'S1))))
           (make-sub '(T3)
                     (list (make-proc-te (make-tuple-te (list 'Number)) 'S2)))) => '((T3 T1) ((-> (* Number) S2) (* Number S1))))
    (test (substitution-combination 
           (make-sub '(T1 T2)
                     (list (make-tuple-te (list 'Number 'S1)) (make-proc-te (make-tuple-te (list 'Number)) 'S4)))
           (make-sub '(T3 T4 T5)
                     (list (make-proc-te (make-tuple-te (list 'Number)) 'S2) 
                           (make-tuple-te (list 'Number 'S1)) 'Boolean))) => '((T5 T4 T3 T1 T2) (Boolean (* Number S1) (-> (* Number) S2) (* Number S1) (-> (* Number) S4)))))



(define (substitution-application-tests) 
    (test (substitution-application 
            (make-sub '(T1 T2 T3) (list 'Number (make-tuple-te (list 'T4 'Number)) 'Boolean))
            (make-tuple-te (list 'T2 (make-proc-te (make-tuple-te (list 'Number 'T1)) 'T1))) ) 
          => '(* (* T4 Number) (-> (* Number Number) Number)))
    (test (substitution-application 
            (make-sub '(S1 S2)
            (list (make-tuple-te (list 'T21
                                       (make-proc-te (make-tuple-te (list 'Number 'T11)) 'T11)))
                  'T3))
            (make-tuple-te (list 'S2 (make-proc-te (make-tuple-te (list 'Number 'S1)) 'S1))) ) 
          => '(* T3 (-> (* Number (* T21 (-> (* Number T11) T11))) (* T21 (-> (* Number T11) T11)))))
)


(run-tests 
     (substitution-combination-tests)
     (substitution-application-tests))