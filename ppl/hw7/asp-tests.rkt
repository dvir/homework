#lang racket

(require "interpreter-core.rkt" "asp.rkt")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 'derive' tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (derive-tests)
  (test (derive 3) => 3)
  (test (derive '(let ((x 1)) (+ x 1))) => 
        '( (lambda (x)(+ x 1)) 1))
  (test (derive '(letrec ((f (lambda(n)(if (= n 0)
                                          1
                                         (* n (f (- n 1)))))
                            ))
                     (f 4)))
        =>
        '((lambda (f) (set-box! f (lambda (n) (if (= n 0) 1 (* n ((unbox f) (- n 1)))))) ((unbox f) 4)) (box 'unassigned))) 
  
  
  
  (test (derive '(letrec ((f1 (lambda (x) (+ x 1)))
                          (f2 (lambda (x y) (if (> x 0) 
                                                 (f2 (- x 1) (+ y 1))
                                                 (f1 y)))))
                    (f2 3 6)))
        => 
        '((lambda (f1 f2) (set-box! f1 (lambda (x) (+ x 1))) (set-box! f2 (lambda (x y) (if (> x 0) ((unbox f2) (- x 1) (+ y 1)) 
                                                                                                         ((unbox f1) y)))) ((unbox f2) 3 6))
          (box 'unassigned)
          (box 'unassigned))
   )

  
  ;old letrec derivation expressions
  ;(test (derive '(while (> (unbox evegeny-n) 0) (set-box! evegeny-n (- (unbox evegeny-n) 1)))) =>
  ;      '((lambda (pred body)
  ;          ((lambda (iter) (set-box! iter (lambda () (if (pred) (begin (body) ((unbox iter))) 'ok))) ((lambda (iter) (iter)) (unbox iter))) (box 'unassigned)))
  ;        (lambda () (> (unbox evegeny-n) 0))
  ;        (lambda () (set-box! evegeny-n (- (unbox evegeny-n) 1)))))  
  

  
  ; new letrec derivation expressions, 
  ; note that if you haven't done question 5 you shoud uncoment tests above and use them to test your while and repeat
  (test (derive '(while (> (unbox evegeny-n) 0) (set-box! evegeny-n (- (unbox evegeny-n) 1)))) =>
        '((lambda (pred body) ((lambda (iter) (set-box! iter (lambda () (if (pred) (begin (body) ((unbox iter))) 'ok))) ((unbox iter))) (box 'unassigned)))
          (lambda () (> (unbox evegeny-n) 0))
          (lambda () (set-box! evegeny-n (- (unbox evegeny-n) 1))))) 
        
)


(define (repeat->iteration-test)
  (letrec ((expr  '(repeat (set-box! evegeny-n (- (unbox evegeny-n) 1)) (> (unbox evegeny-n) 0)))
           (derived-expr (repeat->iteration-expression expr))
           (prefix '(define evegeny-n (box 0)))
           (postfix '(unbox evegeny-n))
           (result (derive-eval (list 'begin prefix derived-expr postfix)))
           (flatten-expression (flatten derived-expr))
           (have-while (member 'while flatten-expression))
           (have-repeat (member 'repeat flatten-expression)))
    (test (list result have-while have-repeat) => '(-1 #f #f))
    ))

(define (repeat->while-test)
  (letrec ((expr  '(repeat (set-box! evegeny-n (- (unbox evegeny-n) 1)) (> (unbox evegeny-n) 0)))
           (derived-expr (repeat->while-expression expr))
           (prefix '(define evegeny-n (box 0)))
           (postfix '(unbox evegeny-n))
           (result (derive-eval (list 'begin prefix derived-expr postfix)))
           (flatten-expression (flatten derived-expr))
           (have-while (not (member 'while flatten-expression)))
           (have-repeat (member 'repeat flatten-expression)))
    (test (list result have-while have-repeat) => '(-1 #f #f))
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Invoking tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(run-tests
 (derive-tests)
 (repeat->iteration-test)
 (repeat->while-test)
 )
