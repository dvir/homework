#lang racket

(require "ass2.rkt" "equation-adt.rkt" "substitution-adt.rkt" "type-expression-adt.rkt")
(provide (all-defined-out))

;;; Infer type of a Scheme expression:

;Signature: infer-type(scheme-expr)
;Type: Client view: [Scheme-expression -> Type-expression]
;      Implmentation view: [LIST union Symbol union Number -> LIST union Symbol]
;Purpose: Infer the type of a scheme expression using the equations method
;Tests: (infer-type '(lambda (f x) (f (f x)))) ==> (-> (* (-> (* T_4) T_4) T_4) T_4)
(define infer-type 
  (lambda (scheme-expr)
    (let ((expr-tvars-list (make-expr-tvars-list scheme-expr)))     
      (get-expression-of-variable
        (solve-equations (make-equations scheme-expr expr-tvars-list))
        (get-var-of-expr expr-tvars-list scheme-expr))
      )
  ))  

;Signature: solve-equations(equation-list)
;Type: Client view: [List(Equation) -> Substitution]
;      Implmentation view: [LIST -> LIST]
;Purpose: Solve the type equations and return the resulting substitution or error, if not solvable
;Tests: (solve-equations
;            (make-equations 
;                    '((lambda(x)(x 11)) (lambda(y) y)) 
;                    (make-expr-tvars-list '((lambda(x)(x 11)) (lambda(y) y))))
;                 ==>
;    '((T_59 T_63 T_57 T_61 T_60 T_62 T_58) 
;      (Number Number Number Number 
;        (-> (* Number) Number) 
;        (-> (* Number) Number) 
;        (-> (* (-> (* Number) Number)) Number)))
(define solve-equations 
  (lambda(equations)
    (solve equations (make-sub '() '()))))


;Signature: solve(equations,substitution)
;Type: Client view: [List(Equation)*Substitution -> Substitution]
;      Implmentation view: [LIST -> LIST]
;Purpose: Solve the equations, starting from a given substitution. return the resulting substitution, or error, if not solvable
;Tests: (solve (make-equations 
;                    '((lambda(x)(x 11)) (lambda(y) y)) 
;                    (make-expr-tvars-list '((lambda(x)(x 11)) (lambda(y) y))))
;                (make-sub '() '())) ==>
;    '((T_59 T_63 T_57 T_61 T_60 T_62 T_58) 
;      (Number Number Number Number 
;       (-> (* Number) Number) 
;       (-> (* Number) Number) 
;       (-> (* (-> (* Number) Number)) Number)))
(define solve 
  (lambda(equations subst)
    (if (null? equations)
        subst    
        (let ((eq (make-equation-from-tes (substitution-application subst (get-left (get-first-equation equations))) 
                                          (substitution-application subst (get-right (get-first-equation equations)))))
             )
          (letrec ((solve-var-eq            ;If one side of eq is a variable 
                    (lambda(var-part other-part) 
                         (solve (cdr equations) 
                                (substitution-combination subst (make-sub (list var-part) (list other-part))))))
                   (both-sides-atomic? 
                    (lambda(eq) (and (atomic? (get-left eq)) (atomic? (get-right eq)))))
                   (handle-both-sides-atomic 
                    (lambda(eq) (if (equal-atomic-te? (get-left eq) (get-right eq)) 
                                    (solve (get-rest-equations equations) subst)
                                    (error 'solve "equation contains unequal atomic types: ~e" eq))))
                  )
            (cond ((variable? (get-left eq)) (solve-var-eq (get-left eq) (get-right eq)))              
                  ((variable? (get-right eq)) (solve-var-eq (get-right eq) (get-left eq))) 
                  ((both-sides-atomic? eq) (handle-both-sides-atomic eq))
                  ((and (composite? (get-left eq)) (composite? (get-right eq)) (unifyable-structure eq)) 
                   (solve (append (get-rest-equations equations) (split-equation eq)) subst))
                  (else (error 'solve "equation contains unknown type expresion: ~e" eq)))
           )))
  ))


;Signature: unifyable-structure(equation)
;Type: Client view: [Equation -> Boolean]
;      Implementation view: [LIST -> Boolean]
;Purpose: Compars the structure of the type expressions of the equation
;Tests: (unifyable-structure '(((-> (* T_3) T_1)) ((-> (* T_2) T_1)))) ==> #t
;         (unifyable-structure '(T_0 (-> (* T_3) T_1)) ==> #f
(define unifyable-structure
  (lambda(eq)
   (let ((left (get-left eq))
         (right (get-right eq)))
     (or (and (procedure? left) (procedure? right) 
              (= (tuple-length (proc-parameter-tuple-tes left)) 
                 (tuple-length (proc-parameter-tuple-tes right))))
         (and (tuple? left) (tuple? right)
              (= (tuple-length left) (tuple-length right))))
     ) 
  ))


;Signature: split-equation(equation)
;Type: Client view: [Equation -> List(Equation)]
;      Implementation view: [LIST -> LIST]
;Purpose: For an equation with unifyable type expressions, create equations for corresponding components.
;Example: (split-equation (make-equation-from-tes '(-> (* T_3) (-> (* T_3) T_1)) 
;                                      '(-> (* T_3) (-> (* T_2) T_2)))) ==>
;==> ( ( (-> (* T_3) T_1) (-> (* T_2) T_2)) (T_3 T_3))
;
;(split-equation 
;              (make-equation-from-tes (make-tuple-te (list 'Number 'Number)) 
;                                      (make-tuple-te (list 'T1 'Number))))
;==>'((Number T1) (Number Number))
;
;(split-equation (make-equation-from-tes (make-proc-te (make-tuple-te (list 'T1 'Number)) 'T2) 
;                                        (make-proc-te (make-tuple-te (list 'Number 'S1)) 'Boolean)))
;==>'((T2 Boolean) (T1 Number) (Number S1))
;Pre-condition: (and (composite? (get-left eq)) (composite? (get-right eq)) (unifyable-structure eq)) = #t
(define split-equation 
  (lambda(eq)
     (let (
           (left (get-left eq))
           (right (get-right eq))
          )
     (cond
        ((empty-equation? eq) (list))
        ((eq? left 'Number) (list))
        ((and (tuple? left) (tuple? right)
              (if (and (null? (tuple-components left)) (null? (tuple-components right)))
                  (list)
                  (map
                     (lambda(te1 te2) (make-equation-from-tes te1 te2))
                     (tuple-components left)
                     (tuple-components right)
                  )
              )
        ))
        ((and (procedure? left) (procedure? right)) 
                        (append 
                         (list (make-equation-from-tes (proc-return-te left) (proc-return-te right)))
                         (split-equation (make-equation-from-tes (proc-parameter-tuple-tes left) (proc-parameter-tuple-tes right)))
                        )         
        )
        ((and (type-expr? left) (type-expr? right)) (make-equation-from-tes left right))
        (else (error 'split-equation "Unknown equation format: ~eq" eq))
      )
    )
))