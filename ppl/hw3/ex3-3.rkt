#lang racket

(require "asp.rkt" "type-expression-adt.rkt" "ass2.rkt")
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Equation ADT
; An equation is implemented as a list of two type expressions.

;;;Constructors:

;Signature: make-equation-from-tes(te1,te2)
;Type: Client view: [Type-expression*Type-expression -> Equation]
;      Implementation view: [LIST union Symbol * LIST union Symbol -> LIST]
;Purpose: equation constructor
;Tests: (make-equation-from-tes (make-proc-te (make-tuple-te (list 'Number 'Number)) 'Number) (make-proc-te (make-tuple-te (list 'Number 'Number)) 'Boolean)) ==> 
;                                   '((-> (* Number Number) Number) (-> (* Number Number) Boolean))
(define make-equation-from-tes
  (lambda (type-expr-l type-expr-r) 
    (attach-tag
        (list type-expr-l type-expr-r)
        'equation
    )
))


;;;Getters:

;Signature: make-empty-equation()
;Type: [Empty -> LIST]
;Purpose: Create an empty equation
;Tests: (make-empty-equation)  ==> '() 
(define make-empty-equation
  (lambda () 
    (attach-tag
        (list)
        'equation   
    )
))


;Signature: get-left(eq)
;Type: Client view: [Equation -> type-expression]
;      Implementation view: [Pair(LIST union Symbol, LIST union Symbol) -> LIST union Symbol]
;Purpose: Get the left type expression of an equation.
;Tests: (get-left(make-equation-from-tes (make-proc-te (make-tuple-te (list 'Number 'Number)) 'Number) (make-proc-te (make-tuple-te (list 'Number 'Number)) 'Boolean)))
; ==> '(-> (* Number Number) Number)
;Pre-condition: eq is a pair
(define get-left
  (lambda (eq) 
    (if (non-empty-equation? eq) (car (get-content eq)) ((display eq)(newline) 'Empty)))
   )

;Signature: get-right(eq)
;Type: Client view: [Equation -> type-expression]
;      Implementation view: [Pair(LIST union Symbol, LIST union Symbol) -> LIST union Symbol]
;Purpose: Get the right type expression of an equation.
;Tests: (get-right(make-equation-from-tes (make-proc-te (make-tuple-te (list 'Number 'Number)) 'Number) (make-proc-te (make-tuple-te (list 'Number 'Number)) 'Boolean))) 
; ==> '(-> (* Number Number) Boolean)
;Pre-condition: eq is a pair
(define get-right
  (lambda (eq) 
    (if (non-empty-equation? eq) (cadr (get-content eq)) 'Empty))
   )


;Signature: get-var-of-expr(expr-tvars-list expr)
;Type: Client view: [LIST(PAIR-LIST(Scheme-expression*Symbol)) -> Symbol]
;      Implementation view: [LIST(LIST(LIST union Symbol * Symbol)) * LIST union Symbol -> Symbol]
;Purpose: Find the type of a Scheme expression in a list of pairs of a Scheme-expression and a type vars
;Tests: (get-var-of-expr '(((lambda (x) (+ x 1)) T_0) ((+ x 1) T_1) (1 T_4) (x T_3) (+ T_2)) '+)  ==> T_2 
(define get-var-of-expr
  (lambda (expr-tvars-list expr)
    (let ((expr-pair (assoc expr expr-tvars-list)))
      (if (and expr-pair (not (null? expr-pair)))
          (cadr expr-pair)
          void))
    ))


;;; Identifiers:

;Signature: equation?(eq)
;Type: [T -> Boolean]
;Purpose: Check whether a given value represents an equation.
;Tests: (equation? (make-equation-from-tes (make-proc-te (make-tuple-te (list 'Number 'Number)) 'Number) (make-proc-te (make-tuple-te (list 'Number 'Number)) 'Boolean))) ==> #t
(define equation?
  (lambda (eq) 
    (and (tagged-by? eq 'equation) (list? (get-content eq)) (type-expr? (car (get-content eq))) (type-expr? (cadr (get-content eq))))
    ))


;Signature: empty-equation?(eq)
;Type: [LIST(LIST union Symbol, LIST union Symbol) -> Boolean]
;Purpose: Check whether a given value represents an empty equation.
;Tests: (empty-equation? '())  ==> #t 
(define empty-equation?
  (lambda (eq)
    (and (tagged-by? eq 'equation) (null? (get-content eq)))
))


;Signature: non-empty-equation?(eq)
;Type: [List(LIST union Symbol, LIST union Symbol) -> Boolean]
;Purpose: Check whether a given value represents a non-empty equation.
;Tests: (non-empty-equation? '())  ==> #f 
(define non-empty-equation?
  (lambda (eq)
    (and (tagged-by? eq 'equation) (not (null? eq)))
))


;;; Constructor for a set of equations:

;Signature: make-equations(Scheme-expression,expression-type-vars-list)
;Type: Client view: [Scheme-expression*LIST(PAIR-LIST(Scheme-expression,Symbol)) -> LIST(Equation)]
;      Implementation view: [LIST union Symbol * LIST(LIST(LIST union Symbol * Symbol)) -> 
;                                                                 LIST(LIST(LIST union Symbol * LIST union Symbol))]
;Purpose: Return a set of equations for a given Scheme expression and a list of pairs: Sub-expression and its type variable
;Tests: (make-equations '(lambda (x) (+ x 1)) '(((lambda (x) (+ x 1)) var_0) ((+ x 1) var_1) (1 var_4) (x var_3) (+ var_2))) 
;    '((var_0 (-> (* var_3) var_1)) (var_2 (-> (* var_3 var_4) var_1)) (var_4 Number) (var_2 (-> (* Number Number) Number)))
(define make-equations
  (lambda (expr expr-tvars-list)
    (filter (lambda(x) (not (empty-equation? x))) 
            (map (lambda (expr) (make-equation expr expr-tvars-list)) 
                 (map car expr-tvars-list)))
 ))

;Signature: make-equation(Scheme-expression,expression-type-vars-list)
;Type: Client view: [Scheme-expression*LIST(PAIR-LIST(Scheme-expression,Symbol)) -> LIST(Equation)]
;      Implementation view: [LIST union Symbol * LIST(LIST(LIST union Symbol * Symbol)) -> 
;                                                                    LIST(LIST union Symbol * LIST union Symbol)]
;Purpose: Return a single equation
;Tests: (make-equation '(lambda (x) (+ x 1)) '(((lambda (x) (+ x 1)) var_0) ((+ x 1) var_1) (1 var_4) (x var_3) (+ var_2))) 
;    '(var_0 (-> (* var_3) var_1))
(define make-equation
  (lambda (se expr-tvars-list)
    (letrec ((get-var-of-expr 
                (lambda (expr)
                  (let ((expr-pair (assoc expr expr-tvars-list)))
                    (if (and expr-pair (not (null? expr-pair)))
                        (cadr expr-pair)
                        void))
                  )))
      (cond ((lambda? se)
             (let ((left (get-var-of-expr se))
                   (right (make-proc-te
                           (make-tuple-te
                            (map get-var-of-expr (lambda-parameters se)))
                           (get-var-of-expr (lambda-body-last-expr se))) )
                   )
                  (make-equation-from-tes left right)))
                   ((application? se)
                    (let ((left (get-var-of-expr (operator se)))
                          (right (make-proc-te
                                  (make-tuple-te
                                   (map get-var-of-expr (operands se)))
                                  (get-var-of-expr se)) )
                          )
                      (make-equation-from-tes left right))) 
                   ((number? se) 
                    (let ((left (get-var-of-expr se))
                          (right 'Number))
                      (make-equation-from-tes left right))) 
                   ((boolean? se) 
                    (let ((left (get-var-of-expr se))
                          (right 'Boolean))
                      (make-equation-from-tes left right)))
                   ((primitive-procedure? se) 
                    (let ((left (get-var-of-expr se))
                          (right (get-primitive-type se)))
                      (make-equation-from-tes left right)))
                   (else (make-empty-equation)))
      )
  ))

(define binary-numeric-primitive-type (list (make-proc-te (make-tuple-te (list 'Number 'Number)) 'Number)))
(define binary-logical-primitive-type (list (make-proc-te (make-tuple-te (list 'Number 'Number)) 'Boolean)))
(define binary-primitive-types (list (list '+ binary-numeric-primitive-type) (list '- binary-numeric-primitive-type) (list '* binary-numeric-primitive-type)
                                                                          (list '> binary-logical-primitive-type)))

;;;Getters for a set of equations:

;Signature: get-first-equation(equations)
;Type: [LIST(LIST union Symbol) -> LIST union Symbol]
;Purpose: Get the first equation of an equation set.
;Tests: (get-first-equation '((T_1 T_2)(T_2 T_3)))  ==> '(T_1 T_2) 
(define get-first-equation
  (lambda (equations) (car equations)))


;Signature: get-rest-equations(equations)
;Type: [LIST(LIST union Symbol) -> LIST(LIST union Symbol)]
;Purpose: Get a subset of equations from a set of equations, starting from the second.
;Tests: (get-rest-equations '((T_1 T_2)(T_2 T_3)))  ==> '(T_2 T_3) 
(define get-rest-equations
  (lambda (equations) (cdr equations)))


;;;Primitive-procedure-management:

;The types of the recognized primitives are given in teh global variable binary-primitive-types

;Signature: primitive-procedure?(se)
;Type: (LIST union Symbol -> Boolean]
;Purpose: Check whether a procedure is a primitive one.
;Tests: (primitive-procedure? '+)  ==> #t 
(define primitive-procedure? 
  (lambda (se)
    (let ((prim-se? (member se (map car binary-primitive-types))))
      (if prim-se? #t #f))
   ))


;Signature: get-primitive-type(se)
;Type: (LIST union Symbol -> LIST union Symbol]
;Purpose: Get the type of a primitive procedure.
;Tests: (get-primitive-type '+)  ==> '(-> (* 'Number 'Number) 'Number)]
;Pre-condition: se is a primitive-procedure
(define get-primitive-type
  (lambda (se)
    (let ((symbol-type-pair (assoc se binary-primitive-types)))
      (if symbol-type-pair
          (cadr symbol-type-pair)
          'Empty))
   ))
