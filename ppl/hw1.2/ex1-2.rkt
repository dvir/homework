;----------------;
;  Question 1.a  ;
;----------------;
; Signature: ln2-rec(n)
; Type: [Number -> Number]
; Purpose: Recursively compute the sum of the first n elements in the ln2 series
; Example: (ln2-rec 4)
; Pre-conditions: n >= 0
; Post-conditions: return sum of first n elements in ln2 series
; Tests: (ln2-rec 2) => 1/2
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
; Signature: third-iter(n, acc)
; Type: Number*Number -> Number
; Purpose: Iteratively compute the sum of the first n elements in the third series (+ acc)
; Example: (third-iter 4 0)
; Pre-conditions: n >= 0
; Post-conditions: return (acc + sum of first n elements in third series)
; Tests: (third-iter 3 0) => 3/8
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
; Signature: sum-alt(term, a, next, b)
; Type: [Number -> Number] * Number * [Number -> Number] * Number -> Number
; Purpose: Compute the sum of the alternating series in the range [a, b], starting from a, jumping with (next a) and adding (term a) to the sum.
; Example: (sum-alt (lambda(x)x) 1 (lambda(x)(+ 1 x)) 3)
; Pre-conditions:
; Post-conditions: return the sum of (term a) - (term (next a)) + (term (next (next a))) ... +- (term (next .. (next a) .. ))
; Tests: (sum-alt (lambda(x)x) 1 (lambda(x)(+ 1 x)) 4) => -2
(define sum-alt
  (lambda(term a next b)
    (if (> a b)
       0
       (+
          (term a)
          (sum-alt 
             (lambda(x) (* -1 (term x)))
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
; Signature: term-ln2(n)
; Type: [Number -> Number]
; Purpose: Compute the term corresponding to n in the ln2 series
; Example: (term-ln2 2)
; Pre-conditions: n != 0
; Post-conditions: return 1/n
; Tests: (term-ln2 2) => 1/2
(define term-ln2
  (lambda(n)
    (/ 1 n)
  )
)

; Signature: next-ln2(n)
; Type: [Number -> Number]
; Purpose: Compute the next number in the ln2 series to be passed to term-ln2
; Example: (next-ln2 2)
; Pre-conditions:
; Post-conditions: return n+1
; Tests: (next-ln2 3) => 4
(define next-ln2
  (lambda(n)
    (+ n 1)
  )
)

; Signature: term-third(n)
; Type: [Number -> Number]
; Purpose: Compute the term corresponding to n in the third series
; Example: (term-third 2)
; Pre-conditions: n >= 0
; Post-conditions: return 1/(2^n)
; Tests: (term-third 3) => 1/8
(define term-third
  (lambda(n)
    (/ 
      1
      (expt 2 n)
    )
  )
)

; Signature: next-third(n)
; Type: [Number -> Number]
; Purpose: Compute the next number in the third series to be passed to term-third
; Example: (next-third 2)
; Pre-conditions: 
; Post-conditions: return n+1
; Tests: (next-third 3) => 4
(define next-third
  (lambda(n)
    (+ n 1)
  )
)

;----------------;
;   Question 2   ;
;----------------;
; Signature: con-func(f, a, flag)
; Type: [Number -> Number] * Number * (Number Union Type) -> [Number -> Number]
; Purpose: Produce g(x) according to f(x), number a and a flag. flag == 0 - g(x) = f(x + a), flag == 1 - g(x) = f(x - a), flag == 2 - g(x) = f(x * a), flag == 3 - g(x) = f(x / a), else - g(x) = f(x).
; Example: (con-func (lambda(x) (x)) 2 0)
; Pre-conditions: (flag != 3) OR (flag == 3 AND a != 0)
; Post-conditions: 
; Tests: ((con-func (lambda(x) (x)) 2 0) 3) => 5
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
