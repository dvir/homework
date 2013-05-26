#lang racket

(require "asp.rkt" "ge-adt.rkt")
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;  SUBSTITUTION-EVALUATOR  ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Type: [<Scheme-exp> -> Evaluator-value]
(define derive-eval
  (lambda (exp)
    (applicative-eval (derive exp))))

; Type: [<Scheme-exp> -> Scheme-type]
(define derive-eval-no-value-tag
  (lambda (exp)
    (let ((val (derive-eval exp)))
      (if (value? val)
          (value-content val)
          val))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main substitution-evaluation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Type: [(<Scheme-exp> union Evaluator-value) -> (Evaluator-value)]
;    Evaluator-value is a Scheme-type value, that is marked as a value. Procedures (user or primnitive)
;                    are distinguished from other values. Numbers and Booleans are also marked.
;    Note that the evaluator does not create closures of the underlying Scheme application.
; Pre-conditions: The given expression is legal according to the concrete syntax.
;                 No derived forms.
;                 Inner 'define' expressions are not legal.
; Post-condition: If the input is an already computed Evaluator-value, then output=input.
(define applicative-eval
  (lambda (exp)
    (cond ((atomic? exp) (eval-atomic exp))  ; Number or Boolean or Symbol or empty
          ((special-form? exp) (eval-special-form exp))
          ((evaluator-value? exp) exp)
          ((application? exp)
           (let ((renamed-exp (rename exp)))
             (apply-procedure (applicative-eval (operator renamed-exp))
                              (list-of-values (operands renamed-exp)))))
          (else (error 'eval "unknown expression type: ~s" exp)))))

; Type: [LIST -> LIST]
(define list-of-values
  (lambda (exps)
    (if (no-operands? exps)
        (list)
        (cons (applicative-eval (first-operand exps))
              (list-of-values (rest-operands exps))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Atomic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define eval-atomic
  (lambda (exp)
    (if (not (variable? exp))
        (make-value exp)
        (lookup-variable-value exp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special form handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define special-form?
  (lambda (exp)
    (or (quoted? exp) (lambda? exp) (definition? exp)
        (if? exp) (begin? exp))))

(define eval-special-form
  (lambda (exp)
    (cond ((quoted? exp) (make-value exp))
          ((lambda? exp) (eval-lambda exp))
          ((definition? exp) (eval-definition exp))
          ((if? exp) (eval-if exp))
          ((begin? exp) (eval-begin exp))
          )))

(define eval-lambda
  (lambda (exp)
    (make-procedure (lambda-parameters exp)
                    (lambda-body exp))))


(define eval-definition
  (lambda (exp)
    (display 'eval-definition!)(newline)(display exp)(newline)
    (add-binding! (make-binding (definition-variable exp)
                                (applicative-eval (definition-value exp))))
    'ok))

(define eval-if
  (lambda (exp)
    (if (true? (applicative-eval (if-predicate exp)))
        (applicative-eval (if-consequent exp))
        (applicative-eval (if-alternative exp)))))

(define eval-begin
  (lambda (exp)
    (eval-sequence (begin-actions exp))))

; (define eval-sequence
;   (lambda (exps)
;     (cond ((sequence-last-exp? exps) (applicative-eval (sequence-first-exp exps)))
;           (else (applicative-eval (sequence-first-exp exps))
;                 (eval-sequence (sequence-rest-exps exps))))))
; 

(define eval-sequence
  (lambda (exps)
    (let ((vals (map (lambda(e) (applicative-eval e))
                    exps)))
      (last vals))
    ))

(define true? 
  (lambda (x)
    (not (false? x))))

(define false? 
  (lambda (x)
     (or (eq? x #f) (equal? x '(value #f)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Value identification and List handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define evaluator-value?
  (lambda (val) (or (value? val)
                    (primitive-procedure? val) (compound-procedure? val))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Application handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Type: [Evaluator-procedure*LIST -> Evaluator-value union Scheme-type]
(define apply-procedure
  (lambda (procedure arguments)
    (cond ((primitive-procedure? procedure)
           (apply-primitive-procedure procedure arguments))
          ((compound-procedure? procedure)
           (let ((parameters (procedure-parameters procedure))
                 (body (rename (procedure-body procedure))))
             (eval-sequence
              (substitute body parameters arguments))))
          (else (error 'apply "Unknown procedure type: ~s" procedure)))))


; Type: [Evaluator-primitive-procedure*LIST -> Evaluator-value]
; Retrieve the primitive implementation, retrieve content of the evaluator value
; arguments, apply and create a new evaluator value.
(define apply-primitive-procedure
  (lambda (proc args)
    (make-value
     (apply (primitive-implementation proc)
            (map (lambda (arg)
                   (cond ((evaluator-value? arg) (value-content arg))
                         ((primitive-procedure? arg) (primitive-implementation arg))
                         ((compound-procedure? arg) 
                           (error 'apply-primitive-procedure 
                                  "primitive-appliled-to-evaluator-procedure: ~s" arg))
                         (else arg)))
                 args)))
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;       RENAMING PROCEDURE       ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Signature: rename(exp)
; Purpose: Consistently rename bound variables in 'exp'.
; Type: [<Scheme-exp> -> <Scheme-exp>]
(define rename
  (letrec ((make-new-names
            (lambda (old-names)
              (if (null? old-names)
                  (list)
                  (cons (gensym) (make-new-names (cdr old-names))))))
           (replace
            (lambda (val-exp)
              (cond ((primitive-procedure? val-exp) val-exp)
                    ((value? val-exp)
                     (if (atomic? (value-content val-exp))
                         val-exp
                         (make-value (map rename (value-content val-exp)))))
                    ((compound-procedure? val-exp)
                     (let* ((params (procedure-parameters val-exp))
                            (new-params (make-new-names params))
                            (renamed-subs-body (map rename (procedure-body val-exp)))
                            (renamed-body (substitute renamed-subs-body params new-params)))
                       (make-procedure new-params renamed-body)))))))
    (lambda (exp)
      (cond ((atomic? exp) exp)
            ((lambda? exp)
             (let* ((params (lambda-parameters exp))
                    (new-params (make-new-names params))
                    (renamed-subs (map rename exp)))
               (substitute renamed-subs params new-params)))  ; replaces free occurrences
            ((evaluator-value? exp) (replace exp))
            (else (map rename exp))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;       SUBSTITUTE PROCEDURES       ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Signature: substitute(exp vars vals)
; Purpose: Consistent replacement of all FREE occurrences of 'vars' in 'exp' by 'vals', respectively.
;          'exp' can be a Scheme expression or an Evaluator value.
; Type: [(<Scheme-exp> union Evaluator-value)*LIST(Symbol)*LIST -> T]
; Pre-conditions: (1) substitute is not performed on 'define' or 'let' expressions
;                     or on expressions containing such sub-expressions.
;                 (2) 'exp' is already renamed. Therefore, var has no bound occurrences in exp.
;                 (3) length(vars)=length(vals)
(define substitute
  (letrec ((substitute-var-val  ; Substitute one variable
            (lambda (exp var val)
              (cond ((variable? exp)
                     (if (eq? exp var)
                         val ; substitute free occurrence of var with val
                         exp))
                    ((or (number? exp) (boolean? exp) (quoted? exp)) exp)
                    ((evaluator-value? exp) (substitute-var-val-in-value exp var val))
                    (else ; expression is a list of expressions, application, cond.
                     (map (lambda (e) (substitute-var-val e var val)) exp)))))
           (substitute-var-val-in-value
            (lambda (val-exp var val)
              (cond ((primitive-procedure? val-exp) val-exp)
                    ((value? val-exp)
                     (if (atomic? (value-content val-exp))
                         val-exp
                         (make-value (map (lambda (e) (substitute-var-val e var val))
                                          (value-content val-exp)))))
                    ((compound-procedure? val-exp)
                     (make-procedure (procedure-parameters val-exp)
                                     (map (lambda (e) (substitute-var-val e var val))
                                          (procedure-body val-exp))))))))
    (lambda (exp vars vals)
      (if (and (null? vars) (null? vals))
          exp
          (substitute (substitute-var-val exp (car vars) (car vals))
                      (cdr vars)
                      (cdr vals))))))



