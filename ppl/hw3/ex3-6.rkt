#lang racket

(provide (all-defined-out))

;Signature: enumerate-tree$(tree, cont)
;Type: [LIST union T -> LIST]
;Purpose: List all leaves of a tree
;Tests: (enumerate-tree$ (list 1 (list 2 3) 4) (lambda(x) x)) ==> '(1 2 3 4)
(define enumerate-tree$ (lambda(tree cont)
      (cond 
          ((null? tree) (cont tree))
          ((not (list? tree)) (list (cont tree)))
          ((list? (car tree)) (enumerate-tree$ (append (car tree) (cdr tree)) cont))
          (else 
              (enumerate-tree$
                 (cdr tree)  
                 (lambda(x) (append 
                               (cont x) 
                               (list (car tree))
                             )
                 )
               )
           )
      )
))

;Signature: replace+$(expr, succ-cont, fail-cont)
;Type: [List union Symbol * [T -> T] * [Empty -> List union Symbol] -> List union Symbol]
;Purpose: Replace the leftmost occurrence of the symbol * in expr by the symbol +
;Tests: (replace+$ (make-proc(make-tuple-te(list 'Number make-proc(make-tuple-te(list 'T1) 'T2)) 'T1))) ==> '(-> (+ num (-> (* T1) T2)) T1)
(define replace+$ (lambda (expr succ-cont fail-cont)
	(cond 
		((null? expr) (fail-cont)) ; empty
		((not (list? expr)) ; leaf in the expr
			(if (eq? expr '*)
				(succ-cont '+)
				(fail-cont)))
		(else ; composite expr
			(replace+$ (car expr)
				(lambda (car-res)
					(succ-cont (cons car-res (cdr expr))))
				(lambda ()
					(replace+$
						(cdr expr)
						(lambda (cdr-res)
						(succ-cont
							(cons (car expr) cdr-res)))
						fail-cont
					)
				)
			)
		)
	)                   
))

;Signature: replace->$(expr, succ-cont, fail-cont)
;Type: [List union Symbol * [T -> T] * [Empty -> List union Symbol] -> List union Symbol]
;Purpose: Replace the rightmost occurrence of the symbol -> in expr by the symbol =>
;Tests: (replace->$ (make-proc(make-tuple-te(list 'Number make-proc(make-tuple-te(list 'T1) 'T2)) 'T1))) ==> '(-> (* num (=> (* T1) T2)) T1)
(define replace->$ (lambda (expr succ-cont fail-cont)
	(cond 
		((null? expr) (fail-cont)) ; empty
		((not (list? expr)) ; leaf in the expr
			(if (eq? expr '->)
				(succ-cont '=>)
				(fail-cont)))
		(else ; composite expr
			(replace->$ (car expr)
				(lambda (car-res)
					(succ-cont (cons car-res (cdr expr))))
				(lambda ()
					(replace->$
						(cdr expr)
						(lambda (cdr-res)
						(succ-cont
							(cons (car expr) cdr-res)))
						fail-cont
					)
				)
			)
		)
	)
))

;Signature: replace->and+(expr)
;Type: [List union Symbol -> List union Symbol]
;Purpose: Replace the rightmost occurrence of the symbol -> in expr by the symbol =>,
;         and the leftmost occurrence of the symbol * in expr by the symbol +. 
;Tests: (replace->and+ (make-proc-te (make-tuple-te (list 'Number (make-proc-te (make-tuple-te (list 'Number)) 'Number) 'Number)) 'Number))
;         ==> '(-> (+ Number (=> (* Number) Number) Number) Number))
(define replace->and+ (lambda(expr)
   (cond
     ((not (list? expr))
       (cond 
         ((eq? expr '*) '+)
         ((eq? expr '->) '=>)
         (else expr)
       )
     )
    (else
       (replace+$ 
          (reverse$ (replace->$ 
                     (reverse$ expr) 
                     (lambda (x) x) 
                     (lambda() (expr))
                    )
          )
          (lambda (x) x)
          (lambda() (expr))
       )
     )
   )
))

;Signature: reverse$(list)
;Type: [LIST -> LIST]
;Purpose: Reverse the order of the elements of the list, and also recursively on each of it's elements
;Tests: (reverse$(list 1 (list2 3) 4)) => '(4 '(3 2) 1)
(define reverse$ (lambda(lst)
  (letrec ((iter$ (lambda (lst cont)
    (cond
     ((null? lst) (cont '()))
     ((list? (car lst))
      (iter$ (cdr lst)
        (lambda (new-tail)
          (iter$ (car lst)
            (lambda (new-head)
              (cont (append new-tail (list new-head))))))))
     (else
      (iter$ (cdr lst)
        (lambda (new-tail)
          (cont (append new-tail (list (car lst)))))))))))
    (iter$ lst (lambda(x) x))
    )))
