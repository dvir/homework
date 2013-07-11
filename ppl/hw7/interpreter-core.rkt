#lang racket

(require "asp.rkt" "env-ds.rkt")
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;  FUNC-ENV-EVALUATOR  ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Type: [<Scheme-exp> -> Scheme-type]
(define derive-eval
  (lambda (exp)
    (env-eval (derive exp) the-global-environment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main functional-environment-evaluation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Type: [<Scheme-exp>*Env -> Scheme-type]
;                      (Number, Boolean, Pair, List, Evaluator-procedure)
;       Note that the evaluator does not create closures of the underlying Scheme application.
; Pre-conditions: The given expression is legal according to the concrete syntax. 
;                 Inner 'define' expressions are not legal.
(define env-eval
  (lambda (exp env)
    (cond ((atomic? exp) (eval-atomic exp env))
          ((special-form? exp) (eval-special-form exp env))
          ((application? exp)
           (apply-procedure (env-eval (operator exp) env)
                            (list-of-values (operands exp) env)))
          (else (error 'eval "unknown expression type: ~s" exp)))))

; Type: [LIST -> LIST]         
(define list-of-values
  (lambda (exps env)
    (if (no-operands? exps)
        '()
        (cons (env-eval (first-operand exps) env)
              (list-of-values (rest-operands exps) env)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Atomic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define eval-atomic
  (lambda (exp env)
    (if (or (number? exp) (boolean? exp) (null? exp))
        exp
        (lookup-variable-value exp env))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special form handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define special-form?
  (lambda (exp)
    (or (quoted? exp) (lambda? exp) (definition? exp)
        (if? exp) (begin? exp) (while? exp) (repeat? exp))))

(define eval-special-form
  (lambda (exp env)
    (cond ((quoted? exp) (text-of-quotation exp))
          ((lambda? exp) (eval-lambda exp env))
          ((definition? exp)
           (if (not (eq? env the-global-environment))
               (error 'eval "non global definition: ~s" exp)
               (eval-definition exp)))
          ((if? exp) (eval-if exp env))
          ((begin? exp) (eval-begin exp env))
          ((repeat? exp) (eval-repeat exp env))
          )))

(define eval-lambda
  (lambda (exp env)
    (make-procedure (lambda-parameters exp)
                    (lambda-body exp)
                    env)))

(define eval-definition
  (lambda (exp)
    (add-binding! (make-binding (definition-variable exp)
                                (env-eval (definition-value exp) the-global-environment)))
    'ok))

(define eval-if
  (lambda (exp env)
    (if (true? (env-eval (if-predicate exp) env))
        (env-eval (if-consequent exp) env)
        (env-eval (if-alternative exp) env))))

(define eval-begin
  (lambda (exp env)
    (eval-sequence (begin-actions exp) env)))


(define eval-while (lambda (exp env)
		(let  ((pred (while-pred exp))
	     	  (body (while-body exp)))
   (letrec ((iter (lambda ()
                    (if (env-eval pred env)
                        (begin
                          (env-eval body env)
                          (iter))
                        'ok))))
     (iter)))))


(define eval-repeat (lambda (exp env)
		(let  ((pred (repeat-pred exp))
                 (body (repeat-body exp)))
   (letrec ((iter (lambda ()
                    (if (env-eval pred env)
                        (begin
                          (env-eval body env)
                          (iter))
                        'ok))))
     (begin (env-eval body env) (iter))))
))
  

; Pre-condition: Sequence of expressions is not empty
(define eval-sequence
  (lambda (exps env)
    (let ((vals (map (lambda (e)(env-eval e env)) exps)))
      (last vals))))


(define true?
  (lambda (x)
    (not (eq? x #f))))

(define false?
  (lambda (x)
    (eq? x #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Application handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Type: [Evaluator-procedure*LIST -> Scheme-type]
(define apply-procedure
  (lambda (procedure arguments)
    (cond ((primitive-procedure? procedure)
           (apply-primitive-procedure procedure arguments))
          ((compound-procedure? procedure)
           (let* ((parameters (procedure-parameters procedure))
                  (body (procedure-body procedure))
                  (env (procedure-environment procedure))
                  (new-env (extend-env (make-frame parameters arguments) env)))
             (if (make-frame-precondition parameters arguments)
                 (eval-sequence body new-env)
                 (error 'make-frame-precondition
                        "violation: # of variables does not match # of values while attempting to create a frame"))))
          (else (error 'apply "unknown procedure type: ~s" procedure)))))



; Type: [Evaluator-primitive-procedure*LIST -> Scheme-type]
; Purpose: Retrieve the primitive implementation, and apply to args.
(define apply-primitive-procedure
  (lambda (proc args)
    (apply (primitive-implementation proc) args)))

