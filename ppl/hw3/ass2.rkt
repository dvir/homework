#lang racket

(require "asp.rkt" "type-expression-adt.rkt")
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code from asignment 2:
(define (tagged-list? exp tag)
  (and (list? exp) (eq? (car exp) tag)))


(define fresh-name
  (let((counter 0))
    (lambda name
      (if(null? name)
         (fresh-name 'T)
         (let((new-name (string->symbol (string-append (symbol->string (car name)) "_" (number->string counter)))))
           (set! counter (+ 1 counter))
           new-name)))))


(define (atomic-var? exp)
  (or (number? exp) (boolean? exp) (symbol? exp) (null? exp)))

(define element-of-pool? 
  (lambda(exp exp-var-set)  
    (cond ((null? exp-var-set) #f)
          ((equal? exp (caar exp-var-set))#t) 
          (else (element-of-pool? exp (cdr exp-var-set))))))


(define first-exp
  (lambda(exp)
    (car exp)))

(define rest-exp
  (lambda(exp)
    (cdr exp)))


(define map-list 
  (lambda(fun exp-list result)
    (if (null? exp-list)
        result
        (map-list fun (rest-exp exp-list) 
                  (if (element-of-pool? (first-exp exp-list) result)                                                         
                      result
                      (fun (first-exp exp-list) result))))))

(define add-to-pool
  (lambda(exp var-pool)
    (list (list exp (fresh-name 'T)))))


(define (quoted? exp) (tagged-list? exp 'quote))
(define (quoted-var exp) (cadr exp))


(define make-expr-tvars-list
  (lambda(exp)
    (letrec ((var-pool (list))
             (findVarList (lambda(exp var-pool)     
                            (cond ((null? exp) (list))
                                  ((atomic-var?  exp) (append  
                                                   (add-to-pool exp var-pool)
                                                   var-pool))
                                  ((quoted? exp) (append (add-to-pool (quoted-var exp) var-pool)
                                                   var-pool))
                                  ((lambda?  exp) (append 
                                                   (add-to-pool exp var-pool)
                                                   (map-list findVarList 
                                                             (lambda-body  exp) var-pool)))                                   
                                  (else  (append 
                                          (add-to-pool exp var-pool)
                                          (map-list findVarList exp var-pool)))))))
      
      (findVarList exp var-pool))))
