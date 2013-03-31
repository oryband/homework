#! /usr/bin/env racket

#lang racket


;--------------;
;  question 3  ;
;--------------;
; Signature: deep-sum-digit (n)
; Type: [Number -> Number]
; Purpose: calculates the sum of digits of the sum of digits up to a number smaller than 10.
; Pre-conditions: n>=0, integer.
; Tests: (test (deep-sum-digit 4526) => 8)
(define (deep-sum-digit n)
  (define (sum-digit n acc)
    (if (= n 0)
      acc
      (sum-digit (quotient n 10) (+ acc (remainder n 10)))))

  (sum-digit (sum-digit n 0) 0))


;--------------;
;  question 4  ;
;--------------;
; Signature: calc-1-e (n)
; Type: [Number -> Number]
; Purpose: Approximates 1/e as the sum of n first elements from n_i=(-1)^i/i!
; Pre-conditions: n>=0, integer
; Tests: (test (calc-1-e 6) => 103/280)
(define (calc-1-e n)
  (define (factorial n)
    (define (factorial-iter n acc)
      (if (= n 0)
        acc
        (factorial-iter (sub1 n) (* n acc))))

    (factorial-iter n 1))

  (define (calc-1-e-iter n acc)
    (if (= n 0)
      acc
      (calc-1-e-iter (sub1 n)
                     ((if (even? n) + -) acc (/ 1 (factorial n))))))

  (calc-1-e-iter (add1 n) 1))
