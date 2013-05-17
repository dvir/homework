#lang racket

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; asp code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Tagged data ;;;;;;;;;;;;;;;;;;;;;;
;; Signature: attach-tag(x, tag)
;; Type: [LIST*Symbol -> LIST]
(define attach-tag
  (lambda (x tag) (cons tag x)))

;; Signature: get-tag(x)
;; Type: LIST -> Symbol
(define get-tag 
  (lambda (x) (car x)))

;; Signature: get-content(x)
;; Type: [LIST -> T]
(define get-content
  (lambda (x) (cdr x)))

;; Signature: tagged-list?(x, tag)
;; Type: [T*Symbol -> Boolean]
(define tagged-by?
  (lambda (x tag)
    (and (tagged-data? x)
         (eq? (get-tag x) tag))))



;Signature: tagged-data?(datum)
;Purpose: Identify tagged-data values
;Type: [T -> Boolean]
(define tagged-data?
  (lambda (datum)
    (and (pair? datum)
         (symbol? (car datum)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(define lambda-body-last-expr
  (lambda (exp)
    (last (lambda-body exp))))

;; variable
(define variable-expr?
  (lambda (exp)
    (symbol? exp)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Application
(define application? 
  (lambda (exp) (list? exp)))

(define operator (lambda (exp) (car exp)))
(define operands (lambda (exp) (cdr exp)))
(define no-operands? (lambda (ops) (null? ops)))
(define first-operand (lambda (ops) (car ops)))
(define rest-operands (lambda (ops) (cdr ops)))
