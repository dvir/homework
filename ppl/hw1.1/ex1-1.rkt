#lang scheme
; Signature: deep-sum-digit(n)
; Type: [Number -> Number]
; Purpose: compute the (deep) sum of digits of a number.
; Pre-conditions: n >= 0
; Tests: (deep-sum-digit 4526) => 8
(define deep-sum-digit (lambda(y)
                         (
                          if (< y 10)
                             (display y)
                             (deep-sum-digit (sum-digit y))
                         )
                       )
)

(define sum-digit-internal (lambda(y sum)
              (
               if (< y 10) 
                  (+ y sum) 
                  (sum-digit-internal 
                      (floor (/ y 10)) 
                      (+ sum (modulo y 10))
                   )
              )
            )
)
(define sum-digit (lambda(y) (sum-digit-internal y 0)))