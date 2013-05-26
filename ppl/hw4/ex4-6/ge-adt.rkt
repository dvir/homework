#lang racket

(require "asp.rkt")
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evaluator types:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Value:
; Type: [LIST -> LIST]
(define make-value
  (lambda (x) (attach-tag (list x) 'value)))

; Type: [T -> Boolean]
(define value?
  (lambda (s) (tagged-by? s 'value)))

; Type: [LIST -> LIST]
(define value-content
  (lambda (s) (car (get-content s))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Primitive procedure:
; Type: [T -> LIST]
(define make-primitive-procedure
  (lambda (proc)
    (attach-tag (list proc) 'primitive)))

; Type: [LIST -> T]
(define primitive-implementation
  (lambda (proc)
    (car (get-content proc))))

; Type: [T -> Boolean]
(define primitive-procedure?
  (lambda (proc)
    (tagged-by? proc 'primitive)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Closure:
; Type: [LIST(Symbol)*LIST -> LIST]
(define make-procedure
  (lambda (parameters body)
    (attach-tag (cons parameters body) 'procedure)))

; Type: [T -> Boolean]
(define compound-procedure?
  (lambda (p)
    (tagged-by? p 'procedure)))

; Type: [LIST -> LIST(Symbol)]
(define procedure-parameters
  (lambda (p)
    (car (get-content p))))

; Type: [LIST -> LIST]
(define procedure-body
  (lambda (p)
    (cdr (get-content p))))

; An identification predicate for procedures -- closures and primitive:
; Type: [T -> Boolean]
(define evaluator-procedure?
  (lambda (p)
    (or (primitive-procedure? p) (compound-procedure? p))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;  Global environment ADT implementation  ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;; Construction ;;;;;;;;;;;;;
; Type: [Unit -> Box([Symbol -> (PAIR(Symbol,T) union {empty})])]
; The global environment mis implemented as a boxed lookup function:
; The ADT type is: [Symbol -> Binding union {empty}]
(define make-the-global-environment
  (lambda ()
    (letrec ((make-frame    ; make-frame creates a lookup procedure: 
                            ; [LIST(Symbol)*LIST -> [Symbol -> PAIR(Symbol,T) union {empty}]] 
              (lambda (variables values)
                (lambda (var)
                  (cond ((empty? variables) empty)
                        ((eq? var (car variables)) 
                         (make-binding (car variables) (car values)))
                        (else ((make-frame (cdr variables) (cdr values))
                               var)))))))
      (let* ((primitive-procedures
              (list (list 'car car)
                    (list 'cdr cdr)
                    (list 'cons cons)
                    (list 'null? null?)
                    (list '+ +)
                    (list '* *)
                    (list '/ /)
                    (list '> >)
                    (list '< <)
                    (list '- -)
                    (list '= =)
                    (list 'list list)
                    (list 'append append)
                    ;;      more primitives
                    ))
             (prim-variables (map car primitive-procedures))
             (prim-values (map (lambda (x) (make-primitive-procedure (cadr x)))
                               primitive-procedures))
             (frame (make-frame prim-variables prim-values)))
      (box  frame)))
    ))

(define the-global-environment (make-the-global-environment))


;;;;;;;;;;; Selection:

; Type: [Symbol -> T]
(define lookup-variable-value
  (lambda (var)
    (let ((b ((unbox the-global-environment) var)))
      (if (empty? b)
          (error 'lookup "variable not found: ~s" var)
          (binding-value b)))
    ))

    

;;;;;;;;;;;;; Mutation:

; Type: [PAIR(Symbol,T) -> Unit]
(define add-binding!
  (lambda (binding)
    (let ((bvar (binding-variable binding))
          (bval (binding-value binding))
          (frame (unbox the-global-environment)))    ; frame is the lookup procedure
      (set-box! the-global-environment 
                (lambda (var)
                  (if (eq? var bvar) 
                      binding
                      (frame var)))))
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bindings

; Type: [Symbol*T -> PAIR)Symbol,T)]
(define make-binding
  (lambda (var val)
    (cons var val)))

; Type: [PAIR(Symbol,T) -> Symbol]
(define binding-variable
  (lambda (binding)
    (car binding)))

; Type: [PAIR(Symbol,T) -> T]
(define binding-value
  (lambda (binding)
    (cdr binding)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

