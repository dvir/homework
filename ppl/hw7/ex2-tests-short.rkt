#lang racket

(require "ex2.rkt")

(require "utils.rkt")
(provide (all-defined-out) (all-from-out "utils.rkt"))

(define counter (box 0))

(define (zc) (set-box! counter 0))

(define (ex2-a-test)
  (letrec ((fib (box
                 (lambda (x)
                   (set-box! counter (+ 1 (unbox counter)))
                   (if (< x 2)
                       1
                       (+
                        ((unbox fib) (- x 1))
                        ((unbox fib) (- x 2))))))))
    (let ((mem-fib (memoize fib)))
      (test (begin (zc)
                   (let* ((ans (mem-fib 10))
                          (count (> (unbox counter) 170))) ; expected 177
                     (cons ans count))) => '(89 . #t))
      (test (begin (zc)
                   (let* ((ans (mem-fib 10))
                          (count (zero? (unbox counter) ))) ; expected 0
                     (cons ans count))) => '(89 . #t))
      (test (begin (zc)
                   (let* ((ans (mem-fib 12))
                          (count (> (unbox counter) 450))) ; expected 465
                     (cons ans count))) => '(233 . #t))
      (test (begin (zc)
                   (let* ((ans (mem-fib 12))
                          (count (zero? (unbox counter)))) ; expected 0
                     (cons ans count))) => '(233 . #t))
      )))

(define (ex2-b-test)
  (letrec ((fib (box
                 (lambda (x)
                   (set-box! counter (+ 1 (unbox counter)))
                   (if (< x 2)
                       1
                       (+
                        ((unbox fib) (- x 1))
                        ((unbox fib) (- x 2))))))))
    (let ((mem-rec-fib (memoize-rec! fib)))
      (test (begin (zc)
                   (let* ((ans (mem-rec-fib 10))
                          (count (< (abs (- 11 (unbox counter))) 3))) ; expected 11
                     (cons ans count))) => '(89 . #t))
      (test (begin (zc)
                   (let* ((ans (mem-rec-fib 10))
                          (count (zero? (abs (unbox counter))))) ; expected 0
                     (cons ans count))) => '(89 . #t))
      (test (begin (zc)
                   (let* ((ans (mem-rec-fib 12))
                          (count (< (abs (unbox counter)) 4))) ; expected 2
                     (cons ans count))) => '(233 . #t))
      (test (begin (zc)
                   (let* ((ans (mem-rec-fib 12))
                          (count (zero? (abs (unbox counter))))) ; expected 0
                     (cons ans count))) => '(233 . #t))
      )))

(run-tests
 (ex2-a-test)
 (ex2-b-test)
 )
