#lang racket

(require "type-expression-adt.rkt")
(require srfi/1)
(provide (all-defined-out))


;;;;;;;;;;; Implementation of the Substitution ADT ;;;;;;;;;;;;;;;;;;
;A substitution is represented as a 2 element list of equal length lists of variables and
;type expression. The empty substitution is the list (() ()).

; Constructors:

;Signature: make-sub(variables, tes)
;Type: Client view: [LIST(Symbol)*LIST(Type-expression) -> Sunstitution]
;      Implementation view: [LIST(Symbol)*LIST(LIST union Symbol)) -> lIST] 
;Purpose: substitution constructor
;Tests: (make-sub '(x y z) 
;                (list 'Number 'Boolean (make-proc-te (make-tuple-te (list 'Number)) 'Number))) ==> 
;                                   ((x y z) (Number Boolean (-> (* Number) Number)))
;         (make-sub '(x y z) 
;                (list 'Number 'Boolean (make-proc-te (make-tuple-te (list 'z)) 'Number))) ==>
;     make-sub: circular substitution: ((x y z) (Number Boolean (-> (* z) Number)))
;         (make-sub '(x y z) (list 'Number 'x (make-proc-te (make-tuple-te (list 'y)) 'Number))) ==>
;                                   ((x y z) (Number x (-> (* y) Number)))
;Prec-condition: (length variables) = (length tes)
(define make-sub
  (lambda (variables tes)
    (if (fold (lambda (b1 b2) (and b1 b2)) #t (map non-circular? variables tes))
        ;if all variable substituting expressions are non-circular
        (list variables tes)
        (error 'make-sub "circular substitution: ~sub" (list variables tes))
    )
))

;Signature: non-circular?(var, te)
;Type: [Symbol*(LIST union Symbol) -> Boolean]
;Purpose: Check wheter susbstitutions is circular
;Tests: (non-circular? 'T (make-proc-te (make-tuple-te (list 'Number)) 'Number)) ==> #t
;         (non-circular? 'T (make-proc-te (make-tuple-te (list 'T)) 'Number)) ==> #f
(define non-circular?
  (lambda (var te)
    (cond ((atomic? te) #t)
          ((variable? te) (not (eq? var te)))
          ((composite? te)
           (fold (lambda (b1 b2) (and b1 b2))
                 #t
                 (flatten
                  (map (lambda (te-element) (non-circular? var te-element))
                       te)))) 
          (else (error 'non-circular? "Bad syntax in Type expression type: ~s" te)))
    ))

; Getters:

;Signature: get-variables(sub)
;Type: [LIST -> LIST(Symbol)] 
;Purpose: get variables of substitution 
;Tests: (get-variables (make-sub '(x y z) 
;                        (list 'Number 'Boolean (make-proc-te (make-tuple-te (list 'Number)) 'Number))) ) ==> 
;                                                                                                     (x y z)
(define get-variables
  (lambda (sub) (car sub)))

;Signature: get-tes(sub)
;Type: [LIST -> LIST(LIST union Symbol)] 
;Purpose: getting type expressions of substitution
;Tests: (get-tes 
;           (make-sub '(x y z) (list 'Number 'Boolean (make-proc-te (make-tuple-te (list 'Number)) 'Number))))  
;                                             ==>  (Number Boolean (-> (* Number) Number))
(define get-tes
  (lambda (sub) (cadr sub)))

;Signature: get-expression-of-variable(sub,var)
;Type: [LIST*Symbol -> LIST union Symbol] 
;Purpose: getting expression of cirtain variable
;Tests: (get-expression-of-variable 
;           (make-sub '(x y z) 
;                     (list 'Number 'Boolean 
;                         (make-proc-te (make-tuple-te (list 'Number)) 'Number)))
;           'y ) ==> Boolean 
;Pre-condition: sub is a 2 element list of equal length lists of variables and tes.
(define get-expression-of-variable
  (lambda (sub var)
    (let ((variables (get-variables sub))
          (tes (get-tes sub)))
      (cond ((null? variables) 'Empty)
            ((eq? var (car variables)) (car tes))
            (else (get-expression-of-variable
                   (make-sub (cdr variables) (cdr tes)) var))))
    ))
          

;;;;;;;;;;;;;;;
;identifiers:

;Signature: sub?(sub)
;Type: [T -> Boolean] 
;Purpose: is substitution
;Tests: (sub? (make-sub '(x y z) 
;                         (list 'Number 'Boolean (make-proc-te (make-tuple-te (list 'Number)) 'Number)))) ==> #t
;         (sub? (make-sub (list) (list))) ==> #t
;         (sub? '()) ==> #f
(define sub?
  (lambda (sub) 
    (and (not (null? sub)) 
         (list? (car sub))
         (not (null? (cdr sub)))
         (null? (cddr sub))
         (let ((variables (get-variables sub))
               (tes (get-tes sub)))
           (and (eq? (length variables) (length tes))
                (equal? (map type-expr? tes)
                        (make-list (length tes) #t)))))
    ))


;Signature: empty-sub?(sub)
;Type: [LIST -> Boolean] 
;Purpose: is the substitution empty
;Tests: (empty-sub? (make-sub '(x y z) 
;                       (list 'Number 'Boolean (make-proc-te (make-tuple-te (list 'Number)) 'Number)))) ==> #f
;         (empty-sub? (make-sub (list) (list))) ==> #t
;Pre-condition: (sub? sub)
(define empty-sub?
  (lambda (sub)
    (and (sub? sub)
         (null? (get-variables sub))
         (null? (get-tes sub)))
    ))
                


; Signature: substitution-application(sub,te)
;Type: Client view: [Substitution*Type-expression -> Type-expression]
;      implementation view: [LIST*(LIST union Symbol) -> LIST union Symbol]
;Purpose: applies the substitution to a given type expression
;Tests: (substitution-application 
;            (make-sub '(T1 T2 T3) (list 'Number (make-tuple-te (list 'T4 'Number)) 'Boolean))
;            (make-tuple-te (list 'T2 (make-proc-te (make-tuple-te (list 'Number 'T1)) 'T1))) ) ==>
;                                               (* (* T4 Number) (-> (* Number Number) Number))
(define substitution-application
  (lambda (sub te)
    (cond 
      ((or (not (type-expr? te)) (not (sub? sub))) #f)
      ((empty-sub? sub) te)
      ((atomic? te) te)
      ((variable? te) (if (eq? 'Empty (get-expression-of-variable sub te)) te (get-expression-of-variable sub te)))
      ((composite? te) (cons (car te) (map (lambda(te) (substitution-application sub te)) (cdr te))))
      (else te)
    )
))


;Signature: substitution-combination(sub1,sub2)
;Type: Client view: [Substitution*Substitution -> Substitution]
;      implementation view: [LIST*LIST -> LIST]
;Purpose: combines to substitution to a single substitution
;Tests: (substitution-combination 
;  (make-sub '(T1 T2 T3) 
;            (list 'S1 
;                  (make-tuple-te (list 'S2 'Number)) 
;                  'Boolean))
;  (make-sub '(S1 S2)
;    (list 
;      (make-tuple-te 
;         (list 'T21
;               (make-proc-te 
;                  (make-tuple-te (list 'Number 'T11)) 
;                  'T11)))
;      'T31))) ==>
;  '((S2 S1 T1 T2 T3) 
;(T31 (* T21 (-> (* Number T11) T11)) (* T21 (-> (* Number T11) T11)) (* T31 Number) Boolean));
(define substitution-combination
  (lambda (sub1 sub2)
    (let (
          (sub1-variables (get-variables sub1))
          (sub1-tes (get-tes sub1))
          (sub2-variables (get-variables sub2))
          (sub2-tes (get-tes sub2))
          )
      (cond
        ; if sub2 has no more variables, return sub1
        ((empty-sub? sub2) sub1)
        ; if the first of sub2 variables is also a variable of sub1, throw it out by constructing a new sub2
        ((member (car sub2-variables) (get-variables sub1))
         (substitution-combination
          sub1
          (make-sub (cdr sub2-variables) (cdr sub2-tes))
          )
         )
        ; else it's a variable that isn't in sub1. apply sub2 to it's te and add it to sub1
        (else
         (substitution-combination
          (make-sub (append 
                        (list (car sub2-variables)) 
                         sub1-variables
                     )
                    (append 
                        (list (car sub2-tes))       
                        (map (lambda(te) (substitution-application sub2 te)) sub1-tes)
                     )
          )              
          (make-sub (cdr sub2-variables) (cdr sub2-tes))
          )
         )
        )
      )
))
        


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Auxiliaries

;Signature: flatten(tree)
;Type: Client view: [Substitution*Substitution -> Substitution]
;      implementation view: [LIST*LIST -> LIST]
;Purpose: Flatten a heterogeneous list
;Tests: (flatten '((1) 2) => '(1 2)
(define flatten
    (lambda (tree)
      (if (not (list? tree)) 
          (list tree)
          (fold append (list) (map flatten tree)))
     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                     