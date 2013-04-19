#!/usr/bin/env racket

#lang racket
;#lang lazy

; 1.a
(define foo
  (lambda (x) (display x) (newline) (+ x 2)))

(define goo
  (lambda (x) (display 5) (newline) (+ x 1)))

(foo (goo 0))


; 2.a.1
(lambda (f n)
  (lambda (x)
    (* n (f x))))

; 2.a.2
(lambda (f n)
  (lambda (x)
    (* n (f x)))
  f 3)

; 2.a.3
(define dg
  (lambda (number)
    (if (< number 10)
      1
      (+ (dg (quotient number 10)) 1))))

(dg ((lambda(dg) dg) 450))
