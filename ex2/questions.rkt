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

; 2.a.4
(display "\n2.a.4:\n")
;(+ (lambda (x) 5) x)  ; Throws error.

; 2.c
(display "\n2.c:\n")
(define number-proc
  (lambda(par)
    (if (number? par)
      (+ par 1)
      (+ (par 1) 1))))

(number-proc 10)


; 3.1.a
(display "\n3.1.a:\n")
(lambda(f)
  (f ((lambda(g y) (+ 1 (g y))) + 10)))

; 3.1.b
(display "\n3.1.b:\n")
(lambda(f)
  (lambda(y)
    (y (f f))))

; 3.1.c
(display "\n3.1.c:\n")
(lambda(x)
  (+ x (x 1)))


; 4.a
(display "\n4.a:\n")
(case
  ((lambda(x) (+ x 3)) 3)   ; key
  ;(lambda(x y) (= x y))    ; pred - obsolete.
  [ (1 2 5) 5 ]             ; clause 1
  [ (4 5 6 7) 9 10 11 12 ]  ; clause 2
  [ else 13 ])              ; else


; 5.a
(display "\n5.a:\n")
#|(let ((gcd_ (lambda(a b)  ; need to use letrec (gcd is primitive, use gcd_).
             (if (= b 0)
               a
               (gcd_ b (modulo a b))))))

  (gcd_ (+ (+ 3 3) (+ 3 3)) 8))|#


; 5.c
(display "\n5.c:\n")
(let ( [gcd_ (lambda(a b gcd_new) (if (= b 0)
                                 a
                                 (gcd_new b (modulo a b) gcd_new)))] )

  (gcd_ (+ (+ 3 3) (+ 3 3)) 8 gcd_))

; 5.e
(display "\n5.e:\n")
(let ( [choose (lambda (n k f)
  (cond
    ((= k 0) 1)
    ((= k n) 1)
    (else (+ (f (- n 1) k f)
             (f (- n 1) (- k 1) f)))))] )

(choose 6 4 choose))
