#lang racket

(require "utils.rkt")

; Load student's solution file
(include "ex1-2.rkt")

(define delta 0.00000001)
; question 1
(define (q1a-tests) 
  (run-tests
   (test (< (abs (- (ln2-rec 1) 1)) delta) => #t)
   (test (< (abs (- (ln2-rec 30.0) 0.6767581376913979)) delta) => #t)))


(define (q1b-tests) 
  (run-tests
   (test (< (abs (- (third-iter 1 0) 0.5)) delta) => #t)
   (test (< (abs (- (third-iter 100 0) 0.3333333333333333)) delta) => #t)))

(define my-term (lambda (x) x))
(define my-next (lambda (x) (+ x 1)))
(define (q1c-tests) 
  (run-tests
   (test (sum-alt my-term 1 my-next 1) => 1)
   (test (sum-alt my-term 1 my-next 17) => 9)))


(define (q1d-tests) 
  (run-tests
   (test (< (abs (- (sum-alt term-third 6 next-third 6) (/ 1 64))) delta) => #t)
   (test (< (abs (- (sum-alt term-ln2 1 next-ln2 30.0) 0.6767581376913979)) delta) => #t)))


; question 2
(define (square x) (* x x))
(define (q2-tests) 
  (run-tests
   (test ((con-func square 3 0) 8) => 121)
   (test ((con-func square 3 1) 8) => 25)
   (test ((con-func sqrt 100 100) 100) => 10)
   )) 

; Run the tests
(q1a-tests)
(q1b-tests)
(q1c-tests)   
(q1d-tests)
(q2-tests) 