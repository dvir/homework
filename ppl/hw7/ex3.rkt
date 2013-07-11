#lang racket

(provide (all-defined-out))

; Signature: make-mbtree(val)
; Type: [T -> Mbtree]
; Purpose: Costructing a mutable binary tree
; Tests: 
(define make-mbtree
  (lambda(val)
    (cons 'mbtree 
          (let ((left (box 'null))
                (right (box 'null))
                (value (box val)))
            (lambda(sel)
              (cond
                ((eq? sel 'get-left) (unbox left))
                ((eq? sel 'get-right) (unbox right))
                ((eq? sel 'get-value) (unbox value))
                ((eq? sel 'set-left) (lambda(x) (set-box! left x)))
                ((eq? sel 'set-right) (lambda(x) (set-box! right x)))
                ((eq? sel 'set-value) (lambda(x) (set-box! value x)))
                (else (error 'unknown-selector))
                )
              )
            )
    )
))


; Signature: make-empty-mbtree()
; Type: [Empty -> Mbtree]
; Purpose: Constructing an empty mbtree
; Tests: (make-empty-mbtree) ==> 'null
(define make-empty-mbtree
  (lambda() 
    'null
))



; Signature: get-left()
; Type: [Mbtree -> Mbtree]
; Purpose:returns the left child of a the non-empty node tree, for an empty tree, t, : get-left(t)='null 
; Tests: (get-left (make-mbtree 1)) ==> 'null
(define get-left
  (lambda(mbtree) 
    (if (empty-mbtree? mbtree)
        'null    
        ((cdr mbtree) 'get-left)
    )
))

; Signature: get-right()
; Type: [Mbtree -> Mbtree]
; Purpose:returns the right child of a the non-empty node tree, for an empty tree, t, : get-right(t)='null 
; Tests: (get-right (make-mbtree 1)) ==> 'null
(define get-right
  (lambda(mbtree) 
    (if (empty-mbtree? mbtree)
        'null    
        ((cdr mbtree) 'get-right)
    )
))

; Signature: get-value()
; Type: [Mbtree -> T]
; Purpose:returns the value of a the non-empty node tree, for an empty tree, t, : get-value(t)='null 
; Tests: (get-value (make-mbtree 1)) ==> 1
(define get-value
  (lambda(mbtree) 
    (if (empty-mbtree? mbtree)
        'null
        ((cdr mbtree) 'get-value)
    )
))


; Signature: set-left!(mbtree,child)
; Type: [Mbtree*Mbtree -> Void]
; Purpose: sets left child of the tree mbtree to be child
; Tests: (set-left! (make-mbtree 1) (make-mbtree 2))
(define set-left!
  (lambda(mbtree child) 
     (((cdr mbtree) 'set-left) child)
))


; Signature: set-right!(mbtree,child)
; Type: [Mbtree*Mbtree -> Void]
; Purpose: sets right child of the tree mbtree to be child
; Tests: (set-right! (make-mbtree 1) (make-mbtree 2))
(define set-right!
  (lambda(mbtree child) 
    (((cdr mbtree) 'set-right) child)
 ))


; Signature:set-value!(mbtree,val)
; Type: [Mbtree*T -> Void]
; Purpose: sets value of the node tree mbtree to be val
; Tests: (set-right! (make-mbtree 1) 2)
(define set-value!
  (lambda(mbtree val) 
    (((cdr mbtree) 'set-value) val)
))

; Signature:empty-mbtree?(mbtree)
; Type: [Mbtree -> Boolean]
; Purpose: Check if the tree is empty tree
; Tests: (empty-mbtree? (make-empty-mbtree) => #t)
(define empty-mbtree?
  (lambda(mbtree) 
    (eq? mbtree 'null)
))

; Signature:mbtree?(mbtree)
; Type: [Mbtree -> Boolean]
; Purpose: Check if the parameter is Mbtree 
; Tests: (mbtree? (make-empty-mbtree) => #t)
(define mbtree?
  (lambda(mbtree) 
    (or (empty-mbtree? mbtree) 
        (and (pair? mbtree) (eq? 'mbtree (car mbtree)))
    )
))

;Signature: set-leftmost-occurrence!(mbtree, old, new)
;Type: [Mbtree * T1 * T2 -> Void]
;Purpose: Set the left most occurrence of “old” to “new”
;returning void in any case.
;Tests: 
;Post-condition:  tree = tree@pre with left most occurrence 
;                 of “old” set to “new”.
(define set-leftmost-occurrence!
  (lambda(tree old new)    
    (cond
      ((empty-mbtree? tree) #f)
      ((eq? old (get-value tree)) (begin (set-value! tree new) #t))
      (else
         (if (set-leftmost-occurrence! (get-left tree) old new)
             #t
             (set-leftmost-occurrence! (get-right tree) old new)
         )
      )
    )
))
