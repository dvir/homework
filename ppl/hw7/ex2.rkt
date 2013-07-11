#lang racket

(provide (all-defined-out))


; Signature: lookup(key, lst)
; Type: [T1*List(Pair(T1*T2))->T2 Union List]
; Purpose: find an element in the list according to it's key
; Tests:
;(lookup 3 (list (cons 1 1) (cons 2 2) (cons 3 3)))=>3
;(lookup 4 (list (cons 1 1) (cons 2 2) (cons 3 3)))=>'()
(define lookup 
  (lambda(key lst)
    (cond ((null? lst) lst)
           ((equal? (caar lst) key) (cdar lst))
           (else (lookup key (cdr lst))))
    ))


; Signature: add-entry(key, lst)
; Type: [T1*T2*List(Pair(T1*T2))->List(Pair(T1*T2))]
; Purpose: adding key val Pair to the list lst
; Tests:
;(add-entry 3 3 (list (cons 1 1) (cons 2 2) (cons 3 3)))=>'((3 . 3) (1 . 1) (2 . 2) (3 . 3))
(define add-entry 
  (lambda(key val lst)
    (cons (cons key val) lst)
    ))


; Signature: memoize(function)
; Type: [Box(T1 → T2) → (T1 → T2)]
; Purpose: Create an optimized version of the parameter procedure
; Tests:
;  > (define fib (box 
;        (lambda(n)			
;            (cond ((= n 0) 0)			
;                  ((= n 1) 1)
;          	(else (+ ((unbox fib) (- n 1)) 
;                           ((unbox fib) (- n 2))))))));   
;	> (define mem-fib (memoize fib))
;	> ; time: a built-in Scheme procedure that measures cpu time
;	> (time (mem-fib 30))
;	 cpu time: 0 real time: 2 gc time: 0
;	> (time (mem-fib 30))
;	 cpu time: 0 real time: 0 gc time: 0
(define memoize
   (lambda (f)				; returns a memoized version of f
      (let ((memo (box '())))
         (lambda (x)
            (let ((match (lookup x (unbox memo))))    		; lookup args
               (if (not (null? match))
                   match                  	; return stored value
                   (let ((value ((unbox f) x))) 	; calculate
			      (begin (set-box! memo (add-entry x value (unbox memo))) 
                                  value
                          )
                   )
               )
            )
         )
      )
   )
)	; store new value

; Signature: memoize-rec!(function)
; Type: [Box(T1 → T2) → (T1 → T2)]
; Purpose: Create an optimized version of the parameter procedure
; Tests:
;  > (define fib (box 
;        (lambda(n)			
;            (cond ((= n 0) 0)			
;                  ((= n 1) 1)
;          	(else (+ ((unbox fib) (- n 1)) 
;                           ((unbox fib) (- n 2))))))));   
;	> (define memoize-rec! (memoize-rec! fib))
;	> ; time: a built-in Scheme procedure that measures cpu time
;	> (time (memoize-rec 30))
;	 cpu time: 0 real time: 2 gc time: 0
;	> (time (memoize-rec 32))
;	 cpu time: 0 real time: 0 gc time: 0

(define memoize-rec!
   (lambda (f)				; returns a memoized version of f
     (letrec ((memo (box '()))
              (old-f (unbox f))
              (new-f
               (lambda (x)
                 (let ((match (lookup x (unbox memo))))    		; lookup args
                   (if (not (null? match))
                       match                  	; return stored value
                       (let ((value (old-f x))) 	; calculate
                         (begin
                           (set-box! memo (add-entry x value (unbox memo)))
                           value
                          )
                        )
                    )
                  )
                )	; store new value
              ))
          (begin (set-box! f new-f)
                 new-f
          )
      )
))
