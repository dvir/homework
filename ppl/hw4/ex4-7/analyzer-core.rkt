#lang racket

(require "env-ds.rkt" "asp.rkt")
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;  FUNC-ENV-ANALYZER  ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Type: [<Scheme-exp> -> [(Env -> Scheme-type)]] 
(define derive-analyze-eval 
  (lambda (exp)
    ((analyze (derive exp)) the-global-environment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main functional-environment-analyzer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Type: [<Scheme-exp> -> [(Env -> Scheme-type)]] 
;                        (Number, Boolean, Pair, List, Evaluator-procedure)
; Pre-conditions: The given expression is legal according to the concrete syntax.
;                  Inner 'define' expressions are not legal.
(define analyze 
  (lambda (exp)
    (cond ((atomic? exp) (analyze-atomic exp))
          ((special-form? exp) (analyze-special-form exp))
          ((application? exp) (analyze-application exp))
          (else (error 'eval "unknown expression type: ~s" exp)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Atomic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define analyze-atomic 
  (lambda (exp)
    (if (or (number? exp) (boolean? exp) (null? exp))
        (lambda (env) exp)
        (lambda (env) (lookup-variable-value exp env)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special form handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define special-form? 
  (lambda (exp)
    (or (quoted? exp) (lambda? exp) (lambda-variadic? exp)
        (definition? exp) (if? exp) (begin? exp))))

(define analyze-special-form 
  (lambda (exp)
    (cond ((quoted? exp) (analyze-quoted exp))
          ((lambda? exp) (analyze-lambda exp))
          ((definition? exp) (analyze-definition exp))
          ((if? exp) (analyze-if exp))
          ((begin? exp) (analyze-begin exp)))
        ))

(define analyze-quoted 
  (lambda (exp)
    (let ((text (text-of-quotation exp)))
      (lambda (env)
         text))))

(define analyze-lambda 
  (lambda (exp)
      (if (not (list? (lambda-parameters exp)))
          (lambda (env)
            (make-procedure-variadic (lambda-variadic-parameters exp) 
                                     (analyze-sequence (lambda-variadic-body exp)) 
                                     env))
          (lambda (env)
            (make-procedure (lambda-parameters exp) 
                            (analyze-sequence (lambda-body exp)) 
                            env))
      )
))

(define analyze-definition 
  (lambda (exp)
    (let ((var (definition-variable exp))
          (val (analyze (definition-value exp))))
      (lambda (env)
        (if (not (eq? env the-global-environment))
            (error 'eval "non global definition: ~s" exp)
            (begin (add-binding! (make-binding var (val the-global-environment)))
                   'ok))))))

(define analyze-if 
  (lambda (exp)
    (let ((pred (analyze (if-predicate exp)))
          (consequent (analyze (if-consequent exp)))
          (alternative (analyze (if-alternative exp))))
      (lambda (env)
        (if (true? (pred env))
            (consequent env)
            (alternative env))))))

(define analyze-begin 
  (lambda (exp)
    (let ((actions (analyze-sequence (begin-actions exp))))
      (lambda (env)
        (actions env)))))

; Pre-condition: Sequence of expressions is not empty
(define analyze-sequence 
  (lambda (exps)
    (let ((procs (map analyze exps)))
      (lambda (env)
        (let ((vals (map (lambda (proc) (proc env)) procs)))
          (last vals))))))

(define true? 
  (lambda (x)
    (not (eq? x #f))))

(define false? 
  (lambda (x)
    (eq? x #f)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Application handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define analyze-application 
  (lambda (exp)
    (let ((application-operator (analyze (operator exp)))
          (application-operands (map analyze (operands exp))))
      (lambda (env)
        (apply-procedure (application-operator env)
                         (map (lambda (operand) (operand env)) application-operands))))))

; Type: [Analyzed-procedure*LIST -> Scheme-type]
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
                 (body new-env)
                 (error 'make-frame-precondition
                       "violation: # of variables does not match # of values while attempting to create a frame"))))
          ((compound-procedure-variadic? procedure)
           (let* ((parameters (procedure-variadic-parameters procedure))
                  (body (procedure-variadic-body procedure))
                  (env (procedure-variadic-environment procedure))
                  (new-env (extend-env (make-frame parameters (list arguments)) env)))
                 (body new-env)))                    
          (else (error 'apply "unknown procedure type: ~s" procedure)))))


; Type: [Evaluator-primitive-procedure*LIST -> Scheme-type]
; Retrieve the primitive implementation, and apply to args.
(define apply-primitive-procedure 
  (lambda (proc args)
    (apply (primitive-implementation proc) args)))


