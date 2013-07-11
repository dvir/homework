 #lang racket

(require "asp.rkt")
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;  Data structures  ;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evaluator Procedure types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
    (attach-tag (list parameters body env) 'procedure)))

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
    (cadr (get-content p))))

; Type: [LIST -> Env]
(define procedure-environment
  (lambda (p)
    (caddr (get-content p))))

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
     (lambda (var)
            (cond ((empty? variables) empty)
                  ((eq? var (car variables)) (make-binding (car variables) (car values)))
                  (else ((make-frame (cdr variables) (cdr values))
                          var))))
    ))


(define make-frame-precondition 
  (lambda (vars vals)
    (= (length vars) (length vals))))


; Frame constructor: ADT type is [Binding*Frame -> Frame]
; Produces a new frame that extends the given frame with the new binding.
; Type: [PAIR(Symbol,T)*[Symbol -> PAIR(Symbol,T) union {empty}] -> 
;                                         [Symbol -> PAIR(Symbol,T) union {empty}]]
(define extend-frame 
  (lambda (binding frame)
    (let ((bvar (binding-variable binding))
          (bval (binding-value binding)))
      (lambda (var)
        (if (eq? var bvar) 
            binding
            (frame var))))
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


; Environment selector: ADT type is [Var*Env -> T]
; Purpose: If the environment is defined on the given variable, selects its value
; Type: [Symbol*LIST(Box([Symbol -> PAIR(Symbol,T) union {empty}])) -> T]
(define lookup-variable-value 
  (lambda (var env)
    (letrec ((defined-in-env        ; ADT type is [Var*Env -> Binding union {empty}]
               (lambda (var env)
                 (if (empty-env? env)
                     env
                     (let ((b ((first-frame env) var)))
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
                (extend-frame binding frame)))
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
                   (list 'not not)
                   (list 'list list)
                   (list 'box box)
                   (list 'unbox unbox)
                   (list 'set-box! set-box!)
                   ;; more primitives
                   ))
           (prim-variables (map car primitive-procedures))
           (prim-values (map (lambda (x) (make-primitive-procedure (cadr x)))
                               primitive-procedures))
           (frame (make-frame prim-variables prim-values)))
    (extend-env frame the-empty-environment))))

(define the-global-environment (make-the-global-environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
