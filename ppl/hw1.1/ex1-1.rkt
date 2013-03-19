;--------------;
;  question 3  ;
;--------------;
; Signature: deep-sum-digit(n)
; Type: [Number -> Number]
; Purpose: compute the (deep) sum of digits of a number.
; Pre-conditions: n >= 0, integer
; Tests: (deep-sum-digit 4526) => 8
(define deep-sum-digit (lambda(y)
                         (
                          if (< y 10)
                             y
                             (deep-sum-digit (sum-digit y))
                         )
                       )
)

; Signature: sum-digit(n)
; Type: [Number -> Number]
; Purpose: compute (simple) sum of digit of a number.
; Pre-conditions: n >= 0, integer
; Tests: (sum-digit 12) => 3
(define sum-digit (lambda(n)
              (
               if (< n 10) 
                  n 
                  (+
                     (sum-digit (floor (/ n 10))) 
                     (modulo n 10)
                  )
              )
            )
)



;--------------;
;  question 4  ;
;--------------;
; Signature: calc-1-e(n)
; Type: [Number -> Number]
; Purpose: Approximates 1/e as the sum of n first elements from n_i=(-1)^i/i!
; Pre-conditions: n >= 0, integer 
; Tests: 
(define calc-1-e (lambda(n) (
                    if (= n 0)
                       1
                       (+
                          (calc-1-e (- n 1))
                          (/
                             (if (= 0 (modulo n 2))
                                 1
                                 -1
                             )
                             (factorial n)
                          )
                       )
                 ) 
))

; Signature: factorial(n)
; Type: [Number -> Number]
; Purpose: Computer the factorial (!) of a number n.
; Pre-conditions: n >= 0, integer
; Tests: (factorial 5) => 120
(define factorial (lambda(n) (
                     if (= n 0)
                        1
                        (* n (factorial (- n 1)))
                  )
))
