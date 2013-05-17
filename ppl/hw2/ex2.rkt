
(provide (all-defined-out))
;Signature: fresh-name()
;Type: [Empty->Symbol]
;Purpose: generating new names
;Tests: (fresh-name)=>'var_0, (fresh-name) => 'var_1
;       Or(fresh-name 'T) => (fresh-name 'T) => 'T_0, (fresh-name 'T) => 'T_1
(define fresh-name
  ((lambda(counter)
    (lambda name
      (if(null? name)
         (fresh-name 'var)
         (let((new-name (string->symbol (string-append (symbol->string (car name)) "_" (number->string counter)))))
           (set! counter (+ 1 counter))
           new-name)))) 0))
   

;Signature: get-tag(x)
;Type: [Tagged-data(T) -> Symbol]
;Purpose: Select the tag from a tagged-data value
;Tests: (get-tag '(lambda(x) (+ x 1))) => 'lambda
(define get-tag (lambda (x) (car x)))


;Signature: tagged-list?(exp, tag)
;Type: [Tagged-data(T)*Symbol -> Boolean]
;Purpose: Identify tagged-data values
;Tests: (tagged-list? '(lambda(x) x) 'lambda) => #t
(define tagged-list?
  (lambda (exp tag)
    (and (list? exp)
         (eq? (get-tag exp) tag))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Variable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Signature: variable?(exp)
;Type: [Tagged-data(T)->Boolean]
;Purpose: Identify variables
;Tests: (variable? 'x) => #t
(define variable? (lambda(exp) (symbol? exp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Atomic data 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Signature: atomic?(exp)
;Type: [Tagged-data(T)->Boolean]
;Purpose: identifying atomic expressions
;Tests: (atomic? '(+ x 1)) => #f, (atomic? 'x) => #t
(define atomic?
  (lambda (exp)
    (or (number? exp) (boolean? exp) (variable? exp) (null? exp))))


; Signature: get-content(exp)
; Type: [LIST -> T]
;Purpose: Select the data from a tagged-data value
;Tests: (get-content '(lambda(x y) (+ x 1) (+ y 2))) => '((+ x 1) (+ y 2))
(define get-content
  (lambda (exp) (cdr exp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Quoted
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Signature: quoted?(exp)
;Type:[[Tagged-data(T)->Boolean]
;Purpose: identifying quoted expressions
;Tests: (quoted? '(quote x)) => #t
(define quoted?
  (lambda (exp)
    (tagged-list? exp 'quote)))

;Signature: text-of-quotation(exp)
;Type: [Tagged-data(T) => Symbol]
;Purpose: Select the data from a tagged-data value
;Tests: (text-of-quotation '(quote x)) => 'x
(define text-of-quotation
  (lambda (exp) (car (get-content exp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Lambda
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Signature: lambda?(exp)
;Type: [Tagged-data(T) => Boolean]
;Purpose: identifying lambda expressions
;Tests:(lambda? '(lambda() 1)) => #t
(define lambda?
  (lambda (exp)
    (tagged-list? exp 'lambda)))


;Signature: lambda-body(exp)
;Type: [Tagged-data(T) => List[Symbol]
;Purpose: Select the lambda body from a lambda expression
;Tests: (lambda-body '(lambda(x y) (+ x 1) (+ y 2))) => '((+ x 1) (+ y 2))
(define lambda-body
  (lambda (exp)
    (cdr (get-content exp))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Your Code

;Signature:generate-type-vars (exp)
;Type: T -> List[List[Symbol] 
;Purpose:returns the sequence of sub-expressions with their type variables 
;Tests:(generate-type-vars '(lambda(x) x))=> '(((lambda (x) x) var_0) (x var_1))

(define generate-type-vars (lambda (exp) 
	(append
		(list (list exp (fresh-name 'T)))
		(cond
			((atomic? exp) (quote ()))
			((quoted? exp) (quote ()))
			; if it's not atomic, it's either lambda or application
			((lambda? exp) (foldl append (quote ()) (map generate-type-vars (lambda-body exp))))
			; it's not atomic or lambda; it's an application
			(else (foldl append (quote ()) (map generate-type-vars exp)))
		)
	)
))