#!/usr/bin/env racket

#lang racket
;#lang lazy


; 1.a
(define foo
  (lambda (x) (display x) (newline) (+ x 2)))

(define goo
  (lambda (x) (display 5) (newline) (+ x 1)))

(display "1:\n")
(foo (goo 0))


; 2.a.1
(display "\n2.a.1:\n")
(lambda (f n)
  (lambda (x)
    (* n (f x))))

; 2.a.2
(display "\n2.a.2:\n")
(lambda (f n)
  (lambda (x)
    (* n (f x)))
  f 3)

; 2.a.3
(display "\n2.a.3:\n")
(define dg
  (lambda (number)
    (if (< number 10)
      1
      (+ (dg (quotient number 10)) 1))))

(dg ((lambda(dg) dg) 450))
