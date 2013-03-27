;----------------;
;  Question 1.a  ;
;----------------;
; Signature:
; Type: 
; Purpose: 
; Example: 
; Pre-conditions:
; Post-conditions:
; Tests: 
(define ln2-rec 
  (lambda(n)
    (if (= n 0)
      0
      (+ 
        (ln2-rec (- n 1))
        (*  
          (if (= (modulo n 2) 0) -1 1 )
          (/ 1 n) 
        )
      )
    )
  )
)

;----------------;
;  Question 1.b  ;
;----------------;
; Signature:
; Type: 
; Purpose: 
; Example: 
; Pre-conditions:
; Post-conditions:
; Tests: 
(define third-iter
  (lambda(n acc)
    (if (= n 0)
        acc
        (third-iter 
           (- n 1)
           (+
              acc
              (*
                (if (= (modulo n 2) 0) -1 1 )
                (/ 
                 1
                 (expt 2 n)
                )
              )
           )
        )
    )
  )
)

;----------------;
;  Question 1.c  ;
;----------------;
; Signature:
; Type: 
; Purpose: 
; Example: 
; Pre-conditions:
; Post-conditions:
; Tests: 
(define sum-alt
  (lambda(term a next b)
    (if (> a b)
       0
       (+
          (*
             (if (= (modulo a 2) 0) -1 1 )
             (term a)
          )
          (sum-alt 
             term
             (next a) 
             next 
             b
          )
       )
    )
  )
)

;----------------;
;  Question 1.d  ;
;----------------;
; Signature:
; Type: 
; Purpose: 
; Example: 
; Pre-conditions:
; Post-conditions:
; Tests: 
(define term-ln2
  (lambda(n)
    (/ 1 n)
  )
)

; Signature:
; Type: 
; Purpose: 
; Example: 
; Pre-conditions:
; Post-conditions:
; Tests: 
(define next-ln2
  (lambda(n)
    (+ n 1)
  )
)

; Signature:
; Type: 
; Purpose: 
; Example: 
; Pre-conditions:
; Post-conditions:
; Tests: 
(define term-third
  (lambda(n)
    (/ 
      1
      (expt 2 n)
    )
  )
)

; Signature:
; Type: 
; Purpose: 
; Example: 
; Pre-conditions:
; Post-conditions:
; Tests: 
(define next-third
  (lambda(n)
    (+ n 1)
  )
)

;----------------;
;   Question 2   ;
;----------------;
; Signature:
; Type: 
; Purpose: 
; Example: 
; Pre-conditions:
; Post-conditions:
; Tests: 
(define con-func
  (lambda(f a flag)
      (cond
         ((= flag 0) (lambda(x) (f (+ x a))))
         ((= flag 1) (lambda(x) (f (- x a))))
         ((= flag 2) (lambda(x) (f (* x a))))
         ((= flag 3) (lambda(x) (f (/ x a))))
         (else (lambda(x) (f x)))
      )
  )
)