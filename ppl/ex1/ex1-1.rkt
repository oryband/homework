;--------------;
;  question 3  ;
;--------------;
; Signature: deep-sum-digit (n)
; Type: [Number -> Number]
; Purpose: Calculates the sum of digits of the sum of digits.
; Pre-conditions: n>=0, integer.
; Tests: (test (deep-sum-digit 4526) => 8)
(define (deep-sum-digit n)

  ; Signature: sum-digit-iter (n acc)
  ; Type: [Number^2 -> Number]
  ; Purpose: Sum of digits iterator, tail-recursive.
  ; Pre-conditions: n>=0, integer.
  ; Tests: (test (sum-digit-iter 4526 0) => 17)
  (define (sum-digit-iter n acc)
    (if (= n 0)
      acc
      (sum-digit-iter (quotient n 10) (+ acc (remainder n 10)))))

  ; Signature: sum-digit (n)
  ; Type: [Number -> Number]
  ; Purpose: Calculates the sum of digits.
  ; Pre-conditions: n>=0, integer.
  ; Tests: (test (sum-digit 4526) => 17)
  (define (sum-digit n) (sum-digit-iter n 0))

  (sum-digit (sum-digit n)))


;--------------;
;  question 4  ;
;--------------;
; Signature: calc-1-e (n)
; Type: [Number -> Number]
; Purpose: Approximates 1/e as the sum of n first elements from n_i=(-1)^i/i!
; Pre-conditions: n>=0, integer
; Tests: (test (calc-1-e 7) => 103/280)
(define (calc-1-e n)

  ; Signature: factorial (n)
  ; Type: [Number -> Number]
  ; Purpose: Calculates factorial.
  ; Pre-conditions: n>=0, integer.
  ; Tests: (test (factorial 4) => 24)
  (define (factorial n)

    ; Signature: factorial-iter (n acc)
    ; Type: [Number^2 -> Number]
    ; Purpose: Factorial iterator, tail recursive.
    ; Pre-conditions: n>=0, integer.
    ; Tests: (test (factorial-iter 4 0) => 24)
    (define (factorial-iter n acc)
      (if (= n 0)
        acc
        (factorial-iter (sub1 n) (* n acc))))

    (factorial-iter n 1))

    ; Signature: calc-1-e-iter (n acc)
    ; Type: [Number^2 -> Number]
    ; Purpose: calc-1-e iterator, tail recursive.
    ; Pre-conditions: n>=0, integer.
    ; Tests: (test (calc-1-e-iter 4 0) => 24)
  (define (calc-1-e-iter n acc)
    (if (= n 0)
      acc
      (calc-1-e-iter (sub1 n)
                     ((if (even? n) + -) acc (/ 1 (factorial n))))))

  (calc-1-e-iter n 1))
