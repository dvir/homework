#lang racket

(require "utils.rkt")
(provide (all-defined-out) (all-from-out "utils.rkt"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Tagged data ;;;;;;;;;;;;;;;;;;;;;;

;Signature: attach-tag(x,tag)
;Type: [Symbol*T -> PAIR(Symbol, T)]
(define attach-tag (lambda (x tag) (cons tag x)))

;Signature: get-tag(tagged)
;Type: PAIR(Symbol,T) -> Symbol
(define get-tag (lambda (tagged) (car tagged)))

;Signature: get-content(tagged)
;Type: [PAIR(Symbol,T) -> T]
(define get-content (lambda (tagged) (cdr tagged)))

;Signature: tagged-data?(datum)
;Type: [T -> Boolean]
(define tagged-data?
  (lambda (datum)
    (and (pair? datum) (symbol? (car datum))) ))

;Signature: tagged-by?(tagged,tag)
;Type: [T*Symbol -> Boolean]
(define tagged-by?
  (lambda (tagged tag)
    (and (tagged-data? tagged)
         (eq? (get-tag tagged) tag))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Atomic data ;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Boolean
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define boolean?
  (lambda (exp)
    (or (eq? exp '#t) (eq? exp '#f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define variable?
  (lambda (exp)
    (symbol? exp)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Atomic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define atomic?
  (lambda (exp)
    (or (number? exp) (boolean? exp) (variable? exp) (null? exp))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Composite expressions;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; quote

(define quoted?
  (lambda (exp)
    (tagged-by? exp 'quote)))

(define text-of-quotation
  (lambda (exp) (car (get-content exp))))

(define make-quote
  (lambda (text)
    (attach-tag (list text) 'quote)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lambda

(define lambda?
  (lambda (exp)
    (tagged-by? exp 'lambda)))

(define lambda-parameters
  (lambda (exp)
    (car (get-content exp))))

(define lambda-body
  (lambda (exp)
    (cdr (get-content exp))))

(define make-lambda
  (lambda (parameters body)
    (attach-tag (cons parameters body) 'lambda)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Case
(define case?
  (lambda (exp)
    (tagged-by? exp 'case)))

;; A constructor for case
(define make-case
  (lambda (control case-clauses)
    (cons 'case case-clauses)))

(define case-control cadr)
(define case-clauses cddr)

;; A constructor for case clauses:
(define make-case-clause
  (lambda (compared actions)
    (cons compared actions)))

(define case-compared car)
(define case-actions cdr)

(define case-first-clause
  (lambda (clauses)
    (car clauses)))

(define case-rest-clauses
  (lambda (clauses)
    (cdr clauses)))

(define case-last-clause?
  (lambda (clauses)
    (and (null? (cdr clauses))
         (eq? (case-compared
               (case-first-clause clauses))
              'else))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; definition
(define definition?
  (lambda (exp)
    (tagged-by? exp 'define)))

(define definition-variable
  (lambda (exp)
    (car (get-content exp))))

(define definition-value
  (lambda (exp)
    (cadr (get-content exp))))

(define make-definition
  (lambda (var value)
    (attach-tag (list var value) 'define)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function definition like (define (foo x y z) ...)

(define function-definition?
  (lambda (exp)
    (and (tagged-by? exp 'define)
         (list? (cadr exp)))))

(define function-definition-variable
  (lambda (exp)
    (caar (get-content exp))))

(define function-definition-parameters
  (lambda (exp)
    (cdar (get-content exp))))

(define function-definition-body
  (lambda (exp)
    (cdr (get-content exp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cond

(define cond? (lambda (exp) (tagged-by? exp 'cond)))

(define cond-clauses (lambda (exp) (cdr exp)))
(define cond-predicate (lambda (clause) (car clause)))
(define cond-actions (lambda (clause) (cdr clause)))

(define cond-first-clause (lambda (clauses) (car clauses)))
(define cond-rest-clauses (lambda (clauses) (cdr clauses)))
(define cond-last-clause? (lambda (clauses) (null? (cdr clauses))))
(define cond-empty-clauses? (lambda (clauses) (null? clauses)))
(define cond-else-clause?
  (lambda (clause) (eq? (cond-predicate clause) 'else)))

;; A constructor for cond clauses:
(define make-cond-clause
  (lambda (predicate exps) (cons predicate exps)))

;; A constructor for cond:
(define make-cond
  (lambda (cond-clauses)
    (attach-tag cond-clauses 'cond)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; if

(define if?
  (lambda (exp) (tagged-by? exp 'if)))

(define if-predicate
  (lambda (exp)
    (car (get-content exp))))

(define if-consequent
  (lambda (exp)
    (cadr (get-content exp))))

(define if-alternative
  (lambda (exp)
    (caddr (get-content exp))))

(define make-if
  (lambda (predicate consequent alternative)
    (attach-tag (list predicate consequent alternative) 'if)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Assignment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define assignment?
  (lambda (exp)
    (tagged-by? exp 'set!)))

(define assignment-variable
  (lambda (exp) (cadr exp)))

(define assignment-value
  (lambda (exp) (caddr exp)))

(define make-assignment
  (lambda (variable value)
    (attach-tag (list variable value) 'set!)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; let

(define let? (lambda (exp) (tagged-by? exp 'let)))

(define let-bindings
  (lambda (exp)
    (car (get-content exp))))

(define let-body
  (lambda (exp)
    (cdr (get-content exp))))

(define let-variables
  (lambda (exp)
    (map car (let-bindings exp))))

(define let-initial-values
  (lambda (exp)
    (map cadr (let-bindings exp))))

(define make-let
  (lambda (bindings body)
    (attach-tag (cons bindings body) 'let)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; letrec

(define letrec?
  (lambda (exp) (tagged-by? exp 'letrec)))

(define letrec-bindings
  (lambda (exp)
    (car (get-content exp))))

(define letrec-body
  (lambda (exp)
    (cdr (get-content exp))))

(define letrec-variables
  (lambda (exp) (map car (letrec-bindings exp))))

(define letrec-initial-values
  (lambda (exp) (map cadr (letrec-bindings exp))))

(define make-letrec
  (lambda (bindings body)
    (attach-tag (cons bindings body) 'letrec)))

(define letrec-binding-variable            
  (lambda (binding) (car binding)))

(define letrec-binding-value               
  (lambda (binding) (cadr binding)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Application

(define application? 
  (lambda (exp) (list? exp)))

(define make-application 
  (lambda (operator operands)
    (cons operator operands)))

(define operator (lambda (exp) (car exp)))
(define operands (lambda (exp) (cdr exp)))
(define no-operands? (lambda (ops) (null? ops)))
(define first-operand (lambda (ops) (car ops)))
(define rest-operands (lambda (ops) (cdr ops)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; begin

(define begin? (lambda (exp) (tagged-by? exp 'begin)))
(define begin-actions (lambda (exp) (get-content exp)))
(define make-begin (lambda (seq) (attach-tag seq 'begin)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sequence
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define make-sequence 
  (lambda (exp1 exp2)
    (cons exp1 exp2)))

(define sequence-last-exp? 
  (lambda (exp) (null? (cdr exp))))

(define sequence-first-exp 
  (lambda (exps)(car exps)))

(define sequence-rest-exps 
  (lambda (exps) (cdr exps)))

(define sequence-empty? 
  (lambda (exp) (null? exp)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;  Derived expression handling  ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Type: [<Scheme-exp> -> <Scheme-exp>]
(define derive
  (lambda (exp)
    (if (atomic? exp)
        exp
        (let ((derived-exp 
               (let ((mapped-derive-exp (map derive exp)))
                 (if (not (derived? exp))
                     mapped-derive-exp
                     (shallow-derive mapped-derive-exp)))
               ))
          (if (equal? exp derived-exp)
              exp
              (derive derived-exp))
          ))))

(define derived?
  (lambda (exp)
    (or (cond? exp) (function-definition? exp) (let? exp) (letrec? exp))))

(define shallow-derive
  (lambda (exp)
    (cond ((cond? exp) (cond->if exp))
          ((function-definition? exp) (function-define->define exp))
          ((let? exp) (let->combination exp))
          ;          ((letrec? exp) (letrec->combination exp))
          ((letrec? exp) (letrec->let exp))    
          (else (error 'shallow-derive "unhandled derivation: ~s" exp)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Deriv handlers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define cond->if
  (lambda (exp)
    (letrec ((sequence->exp
              (lambda (seq)
                (cond ((sequence-empty? seq) seq)
                      ((sequence-last-exp? seq) (sequence-first-exp seq))
                      (else (make-begin seq)))))
             (expand-clauses
              (lambda (clauses)
                (if (cond-empty-clauses? clauses)
                    '#f                          ; no else clause
                    (let ((first (cond-first-clause clauses))
                          (rest (cond-rest-clauses clauses)))
                      (if (cond-else-clause? first)
                          (if (cond-empty-clauses? rest)
                              (sequence->exp (cond-actions first))
                              (error 'cond-if "ELSE clause isn't last: ~s"
                                     clauses))
                          (make-if (cond-predicate first)
                                   (sequence->exp (cond-actions first))
                                   (expand-clauses rest))))))))
      (expand-clauses (cond-clauses exp)))))

(define function-define->define
  (lambda (exp)
    (let ((var (function-definition-variable exp))
          (params (function-definition-parameters exp))
          (body (function-definition-body exp)))
      (make-definition var (make-lambda params body)))))

(define let->combination
  (lambda (exp)
    (let ((vars (let-variables exp))
          (body (let-body exp))
          (initial-vals (let-initial-values exp)))
      (make-application (make-lambda vars body) initial-vals))))


(define letrec->combination
  (lambda (exp)
    (letrec ((make-body (lambda (vars vals)
                          (if (null? vars)
                              (letrec-body exp)
                              (make-sequence (make-assignment (car vars)
                                                              (car vals))
                                             (make-body (cdr vars)
                                                        (cdr vals)))))))
      (let* ((vars (letrec-variables exp))
             (vals (letrec-initial-values exp))
             (dummies (make-list (length vals) 'unassigned)))
        (make-application (make-lambda vars (make-body vars vals)) dummies)))))


(define letrec->let
  (lambda (exp)
    (letrec ((make-body (lambda (vars vals)
                          (if (null? vars)
                              (letrec-body exp)
                              (make-sequence (make-assignment (car vars)
                                                              (car vals))
                                             (make-body (cdr vars)
                                                        (cdr vals))))))
             (make-bindings (lambda (vars) 
                              (map (lambda (var) (list var 'unassigned))
                                   vars)))
             )
      (let* ((vars (letrec-variables exp))
             (vals (letrec-initial-values exp))
             )
        (make-let (make-bindings vars) (make-body vars vals))))
    ))

