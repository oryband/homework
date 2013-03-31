;----------------;
;  Question 1.a  ;
;----------------;
; Signature: ln2-rec (n)
; Type: [Number -> Number]
; Purpose: Calculates ln2 up to n-element precision.
; Example: (ln2-rec 10)
; Pre-conditions: n>=1, integer.
; Tests: (test (ln2-rec 5) => 37/60)
(define (ln2-rec n)

  ; Signature: ln2-rec-iter (n acc)
  ; Type: [Number^2 -> Number]
  ; Purpose: ln2-rec iterator, tail-recursive.
  ; Example: (ln2-rec 10 1)
  ; Pre-conditions: n>=0, integer.
  ; Tests: (test (ln2-rec 3) => 5/6)
  (define (ln2-rec-iter n acc)
    (if (= n 0)
      acc
      (ln2-rec-iter (sub1 n)
                    ((if (odd? n) + -) acc (/ 1 n)))))

  (ln2-rec-iter n 0))


;----------------;
;  Question 1.b  ;
;----------------;
; Signature: third-iter (n acc)
; Type: [Number^2 -> Number]
; Purpose: Calculates 1/3 up to n-th element precision.
; Example: (third-iter 10 0)
; Pre-conditions: n>=1, acc>=0, integers.
; Tests: (test (third-iter 3 0) => 11/32)
(define (third-iter n acc)
  (if (= n 0)
    acc
    (third-iter (sub1 n)
                ((if (odd? n) + -) acc (/ 1 (expt 2 n))))))


;----------------;
;  Question 1.c  ;
;----------------;
; Signature: sum-alt (term a next b)
; Type: [Procedure, Number, Procedure, Number -> Number]
; Purpose: Calculates an alternating series.
; Example: (sum-alt (lambda(x) x) 1 (lambda(x) (+ 1 x)) 3)
; Pre-conditions: a,b integers; term,next procedures.
; Tests: (test (sum-alt (lambda(x) x) 1 (lambda(x) (+ 1 x)) 3) => 2)
(define (sum-alt term a next b)

  ; Signature: sum-alt-iter (term a next b acc)
  ; Type: [Procedure, Number, Procedure, Number, Number -> Number]
  ; Purpose: sum-alt iterator, tail-recursive.
  ; Example: (sum-alt-iter (lambda(x) x) 1 (lambda(x) (+ 1 x)) 3 0)
  ; Pre-conditions: acc>=0; a,b,acc integers; term,next procedures.
  ; Tests: (test (sum-alt (lambda(x) x) 1 (lambda(x) (+ 1 x)) 3 0) => 2)
  (define (sum-alt-iter term a next b acc)
    (if (> a b)
      acc
      (sum-alt-iter (lambda(x) (- (term x)))
                    (next a)
                    next
                    b
                    (+ acc (term a)))))

  (sum-alt-iter term a next b 0))


;----------------;
;  Question 1.d  ;
;----------------;
; Signature: term-ln2 (n)
; Type: [Number -> Number]
; Purpose: Defines the ln2 series member.
; Example: (term-ln2 (3))
; Pre-conditions: n>=0, integer.
; Tests: (test (term-ln2 3) => 1/3)
(define (term-ln2 n) (/ 1 n))

; Signature: next-ln2 (n)
; Type: [Number -> Number]
; Purpose: Defines the next ln2 series variable.
; Example: (next (3))
; Pre-conditions: n>=0, integer.
; Tests: (test (next 3) => 4)
(define (next-ln2 n) (add1 n))


; Signature: term-third (n)
; Type: [Number -> Number]
; Purpose: Defines the 1/3 series member.
; Example: (term-third (3))
; Pre-conditions: n>=0, integer.
; Tests: (test (term-third 3) => 8)
(define (term-third n) (/ 1 (expt 2 n)))

; Signature: next-third (n)
; Type: [Number -> Number]
; Purpose: Defines the next 1/3 series variable.
; Example: (next (3))
; Pre-conditions: n>=0, integer.
; Tests: (test (next 3) => 4)
(define (next-third n) (add1 n))


;----------------;
;   Question 2   ;
;----------------;
; Signature: con-func (f a flag)
; Type: [Procedure, Number, Number -> Procedure]
; Purpose: Returns an arithmetic function defined by arguments.
; Example: ((con-func square 2 3) 8)
; Pre-conditions: 0<=flag<=3; f function; a,flag integers.
; Tests: (test ((con-func square 2 3) 8) => 16)
(define (con-func f a flag)
  (cond
    ((= flag 0) (lambda(x) (f (+ x a))))
    ((= flag 1) (lambda(x) (f (- x a))))
    ((= flag 2) (lambda(x) (f (* x a))))
    ((= flag 3) (lambda(x) (f (/ x a))))
    (else (lambda(x) (f x)))))
