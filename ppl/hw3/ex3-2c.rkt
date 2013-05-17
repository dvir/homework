#lang racket

(require "asp.rkt")
(provide (all-defined-out))

;;;;;;;;;;;  Implementation of the Type-Expression ADT ;;;;;;;;;;;;;;;;;;

;;; Type grammar:
; Type -> Atomic | Composite | Type-variable
; Atomic -> ‘Number’ | ‘Boolean’
; Composite -> Procedure | Tuple
; Procedure -> ‘[‘Tuple’ ->’ Type’]’
; Tuple -> (Type ‘*’) Type | ‘Empty’
; Type-variable -> a Symbol

; Constructors:
;Number, Boolean

;Signature: make-proc-te(tuple-te, te)
;Type: [List*(List union Symbol) -> Pair] 
;Purpose: making procedure type expression
;Tests: (make-proc-te (make-tuple-te (list 'Number)) 'Number) ==> (-> (* Number) Number) 
(define make-proc-te
  (lambda (tuple-te te)
    (attach-tag
        (lambda (sel) 
          (sel tuple-te te)
         )
        '->
    )
))

;Signature: make-tuple-te(te-list)
;Type: [LIST -> lIST] 
;Purpose: get tuple
;Tests: (make-tuple-te (list 'Number (make-proc-te (make-tuple-te (list 'Number)) 'T1))) 
;                                                           ==> (* Number (-> (* Number) T1)) 
(define make-tuple-te
  (lambda (te-list)
    (cons '* te-list)))

; Getters:

;Signature: get-constructor(te)
;Type: [LIST union Symbol -> Symbol] 
;Purpose: get type costructor
;Tests: (get-constructor (make-tuple-te (list 'Number 'Number))) ==> *
(define get-constructor
  (lambda (te)
     (cond ((procedure? te) '->)
           ((composite? te) (car te))
           (else te)
      )
))

;Signature: tuple-components(te)
;Type: [LIST union Symbol -> LIST] 
;Purpose: get tuple components
;Tests: (tuple-components (make-tuple-te (list 'Number 'Number))) ==> (Number Number)
(define tuple-components
  (lambda (te)
    (if (tuple? te)
        (cdr te)
        (list))))

;Signature: tuple-length(te)
;Type: [LIST union Symbol -> Number] 
;Purpose: get tuple length
;Tests: (tuple-length (make-tuple-te (list 'Number 'Number))) ==> 2
(define tuple-length
  (lambda (te)
    (if (tuple? te)
        (length (tuple-components te))
        (list))))

;Signature: proc-parameter-tuple-tes(te)
;Type: [LIST union Symbol -> LIST] 
;Purpose: get procedure parameter tuples
;Tests: (proc-parameter-tuple-tes (make-proc-te (make-tuple-te (list 'Number)) 'T1)) ==> '(* Number)
;Pre-condition: (procedure? te)
(define proc-parameter-tuple-tes
  (lambda (te)
    (if (procedure? te)
        ((get-content te) (lambda(tuple-te te) tuple-te))
        #f
    )
))

;Signature: proc-parameter-tes(te)
;Type: [LIST union Symbol -> LIST] 
;Purpose: get procedure parameters
;Tests: (proc-parameter-tes (make-proc-te (make-tuple-te (list 'Number)) 'T1)) ==> '(Number)
;Pre-condition: (procedure? te)
(define proc-parameter-tes
  (lambda (te)
    (if (procedure? te)
        ((get-content te) (lambda(tuple-te te) (cdr tuple-te)))
        #f
    )
))

;Signature: proc-return-te(te)
;Type: [LIST union Symbol -> LIST union Symbol] 
;Purpose: get procedure return type expresison
;Tests: (proc-return-te (make-proc-te (make-tuple-te (list 'Number)) 'T1)) ==> 'T1
;Pre-condition: (procedure? te)
(define proc-return-te
  (lambda (te)
    (if (procedure? te)
        ((get-content te) (lambda(tuple-te te) te))
        #f
    )
))


;Signature: equal-atomic-te?(te1 te2)
;Type: [LIST union Symbol * LIST union Symbol -> Boolean] 
;Purpose: are to type exressions equal
;Tests: (equal-atomic-te? 'Number 'Number) ==> #t
(define equal-atomic-te?
  (lambda (te1 te2)
    (and (symbol? te1) (symbol? te2) (eq? te1 te2))))

;;;;;;;;;;;;;;;
;identifiers:

;Signature: type-expr?(te)
;Type: [LIST union Symbol -> Boolean]
;Purpose: is type expresion
;Tests: (type-expr?(make-proc-te (make-tuple-te (list 'Number)) 'T1)) ==> #t
(define type-expr?
  (lambda (te) 
    (or (atomic? te)(variable? te) (composite? te))))

;Signature: atomic?(te)
;Type: [LIST union Symbol -> Boolean ]
;Purpose: is atomic
;Tests: (atomic? (make-proc-te (make-tuple-te (list 'Number)) 'T1)) ==> #f
(define atomic?
  (lambda (te) 
    (or (eq? te 'Number)(eq? te 'Boolean))))

;Signature: composite?(te)
;Type: [LIST union Symbol -> Boolean 
;Purpose: is composite
;Tests: (composite? (make-proc-te (make-tuple-te (list 'Number)) 'T1)) ==> #t
(define composite?
  (lambda (te) (or (procedure? te) (tuple? te))))

;Signature: tuple?(te)
;Type: [LIST union Symbol -> Boolean ]
;Purpose: is tuple
;Tests: (tuple? (make-proc-te (make-tuple-te (list 'Number)) 'T1)) ==> #f
(define tuple?
  (lambda (te) 
    (and (list? te)(eq? (car te) '*)) ))

;Signature: procedure?(te)
;Type: [LIST union Symbol -> Boolean 
;Purpose: is procedure
;Tests: (procedure? (make-proc-te (make-tuple-te (list 'Number)) 'T1)) ==> #t
(define procedure?
  (lambda (te) 
    (and (tagged-by? te '->))
 ))

;Signature: variable?(te)
;Type: [LIST union Symbol -> Boolean 
;Purpose: is variable
;Tests: (variable? (make-proc-te (make-tuple-te (list 'Number)) 'T1)) ==> #t
(define variable?
  (lambda (te) 
    (and (not (atomic? te))(symbol? te))
   ))



