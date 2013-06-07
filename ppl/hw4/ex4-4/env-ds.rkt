#lang racket

(require "asp.rkt")
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;  Data structures  ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evaluator Procedure types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
; Type: [LIST(Symbol)*LIST*Env -> LIST]
(define make-procedure
  (lambda (parameters body env)
    (attach-tag 
      (lambda (sel)
        (cond
          ((eq? sel 'parameters) parameters)
          ((eq? sel 'body) body)
          ((eq? sel 'env) env)
          ((eq? sel 'parameter-less?) (null? parameters))
          (else (error 'make-procedure "Unknown selector: ~sel" sel))  
        )
      )
      'procedure
    )
))

; Type: [T -> Boolean]
(define compound-procedure?
  (lambda (p)
    (tagged-by? p 'procedure)))

; Type: [LIST -> LIST(Symbol)]
(define procedure-parameters
  (lambda (p)
    ((get-content p) 'parameters)
))

; Type: [LIST -> LIST]
(define procedure-body
  (lambda (p)
    ((get-content p) 'body)
))

; Type: [LIST -> Env]
(define procedure-environment
  (lambda (p)
    ((get-content p) 'env)
))

; Type: [Procedure -> T]
(define parameter-less?
  (lambda (p)
    ((get-content p) 'parameter-less?)
))


;Purpose:  An identification predicate for procedures -- closures and primitive:
; Type: [T -> Boolean]
(define procedure?
  (lambda (p)
    (or (primitive-procedure? p) (compound-procedure? p))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evaluator environment types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Frames
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Frame constructor: ADT type is: [[LIST(Symbol)*LIST -> Frame]
; A frame is a mapping function from variables (symbols) to values. It is implemented as
; a procedure from a Symbol to its binding (a variable-value pair) or to the 'empty' value, 
; in case that the frame is not defined on the given variable.
; Type: [LIST(Symbol)*LIST -> [Symbol -> (PAIR(Symbol,T) union {empty})]]
(define make-frame
  (lambda (variables values)
  (cond 
    ((empty? variables) null)  
    (else 
       (cons
          (make-binding (car variables) (car values))
          (make-frame (cdr variables) (cdr values))
       )      
    )
  )
))

; Signature: get-all-vars(frame)
; Type: [Frame -> LIST(Symbol)]
; Purpose: Get all variables in a frame.
(define get-all-vars 
  (lambda (frame)
    (cond
      ((empty? frame) null)
      (else
        (cons
           (binding-variable (car frame))
           (get-all-vars (cdr frame))
        ) 
      )
    )
))

; Signature: get-all-vals(frame)
; Type: [Frame -> LIST]
; Purpose: Get all values in a frame.
(define get-all-vals 
  (lambda (frame)
    (cond
      ((empty? frame) null)
      (else
        (cons
           (binding-value (car frame))
           (get-all-vals (cdr frame))
        ) 
      )
    )
))

(define make-frame-precondition 
  (lambda (vars vals)
    (= (length vars) (length vals))))


; Frame identification predicate
(define empty-frame? (lambda (frame) (null? frame)))


; Frame replacement: ADT type is [Binding*Frame -> Frame]
; Produces a new frame that extends the given frame with the new binding.
; Type: [PAIR(Symbol,T)*[Symbol -> PAIR(Symbol,T) union {empty}] -> 
;                                         [Symbol -> PAIR(Symbol,T) union {empty}]]
(define add-binding-to-frame-old
  (lambda (binding frame)
    (let ((bvar (binding-variable binding))
          (bval (binding-value binding)))
      (lambda (var)
        (if (eq? var bvar) 
            binding
            (frame var))))
    ))

(define add-binding-to-frame 
  (lambda (binding frame)
    (cons binding frame)
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Environment constructor: ADT type is [Frame*Env -> Env]
; An environment is implemented as a list of boxed frames. The box is needed because the 
; first frame, i.e., the global environment, is changed following a variable definition. 
; Type: [[Symbol -> PAIR(Symbol,T) union {empty}]*
;       LIST(Box([Symbol -> PAIR(Symbol,T) union {empty}])) ->
;                                        LIST(Box([Symbol -> PAIR(Symbol,T) union {empty}]))]
(define extend-env 
  (lambda (frame base-env)
    (cons (box frame) base-env)))

; Environment selectors
; Input type is an environment, i.e., LIST(Box([Symbol -> PAIR(Symbol,T) union {empty}]))
(define enclosing-env (lambda (env) (cdr env)))
(define first-boxed-frame (lambda(env) (car env)))
(define first-frame (lambda(env) (unbox (first-boxed-frame env))))

(define frame-binding-lookup
  (lambda (frame var)
    (cond
      ((empty? frame) null)
      ((eq? (binding-variable (car frame)) var) (car frame))
      (else (frame-binding-lookup (cdr frame) var))
    )  
))


; Environment selector: ADT type is [Var*Env -> T]
; Purpose: If the environment is defined on the given variable, selects its value
; Type: [Symbol*LIST(Box([Symbol -> PAIR(Symbol,T) union {empty}])) -> T]
(define lookup-variable-value 
  (lambda (var env)
    (letrec ((defined-in-env        ; ADT type is [Var*Env -> Binding union {empty}]
               (lambda (var env)
                 (if (empty-env? env)
                     env
                     (let ((b (frame-binding-lookup (first-frame env) var)))
                       (if (empty? b)
                           (defined-in-env var (enclosing-env env))
                           b))))))
      (let ((b (defined-in-env var env)))
        (if (empty? b)
            (error 'lookup "variable not found: ~s\n  env = ~s" var env)
            (binding-value b))))
    ))

; Environment identification predicate
; Type: [T -> Boolean]
(define empty-env? 
  (lambda (env)
    (eq? env the-empty-environment)))

; Global environment mutator: ADT type is [Binding -> Unit]
; Type: [PAIR(Symbol,T) -> Unit]
; Note: Mutation is enabled only for the global environment
(define add-binding! 
  (lambda (binding)
    (let ((frame (first-frame the-global-environment)))
      (set-box! (first-boxed-frame the-global-environment)
                (add-binding-to-frame binding frame)))
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;  Global environment construction  ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define the-empty-environment '())

; Type [Unit -> LIST(Box([Symbol -> PAIR(Symbol,T) union {empty}]))]
(define make-the-global-environment
  (lambda ()
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
                   ;; more primitives
                   ))
           (prim-variables (map car primitive-procedures))
           (prim-values (map (lambda (x) (make-primitive-procedure (cadr x)))
                               primitive-procedures))
           (frame (make-frame prim-variables prim-values)))
    (extend-env frame the-empty-environment))))

(define the-global-environment (make-the-global-environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
