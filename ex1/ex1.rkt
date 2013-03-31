#! /usr/bin/env racket

#lang racket


;--------------;
;  question 3  ;
;--------------;
; Signature: deep-sum-digit(n)
; Type: [Number -> Number]
; Purpose: calculates the sum of digits of the sum of digits up to a number smaller than 10.
; Pre-conditions: n>=0, integer.
; Tests: (test (deep-sum-digit 4526) => 8)
(define (deep-sum-digit n) (sum-digit (sum-digit n 0) 0))

; Signature: sum-digit(n acc)
; Type: [Number, Number -> Number]
; Purpose: sum-digit iterator, tail recursive.
; Pre-conditions: n,acc>=0, integers.
(define (sum-digit n acc)
  (if (= n 0)
    acc
    (sum-digit (quotient n 10) (+ acc (remainder n 10)))))


;--------------;
;  question 4  ;
;--------------;
; Signature:
; Type:
; Purpose: Approximates 1/e as the sum of n first elements from n_i=(-1)^i/i!
; Pre-conditions: n>=0, integer
; Tests:
(define calc-1-e
  (lambda (n)
    (...)))
