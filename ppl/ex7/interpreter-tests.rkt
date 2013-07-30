#lang racket

(require "interpreter-core.rkt" "asp.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Primitive procedures tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define (primitive-tests)
  (test (derive-eval '(* 3 4)) => 12)
  (test (derive-eval '(+ 3 4)) => 7)
  (test (derive-eval '(- 3 4)) => -1)
  (test (derive-eval '(/ 4 2)) => 2)
  (test (derive-eval '(null? (list))) => #t)
  (test (derive-eval '(> 3 4)) => #f)
  (test (derive-eval '(< 3 4)) => #t)
  (test (derive-eval '(= 3 4)) => #f)
  (test (derive-eval '(car (list 3 4))) => 3)
  (test (derive-eval '(cdr (list 3 4))) => '(4))
  (test (derive-eval '(cons 3 (cons 4 (list)))) => '(3 4))
  ;; New tests
  (test (derive-eval '(list 3 4)) => '(3 4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Application and `lambda' tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (app-lambda-tests)
  (test (derive-eval '((lambda (x) x) 12)) => 12)
  (test (derive-eval '((lambda (x y z) (+ x y z)) 12 13 14)) => 39)
  (test (derive-eval '((lambda (x) ((lambda (x) (+ x 1)) 2)) 12)) => 3)
  (test (derive-eval '((lambda (f x y) (f x y)) + 12 4)) => 16)
  (test (derive-eval '((lambda (f x y) (f x y)) ((lambda () +)) 12 4)) => 16))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `begin' tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (begin-tests)
  (test (derive-eval '(begin 1 2 3)) => 3)
  (test (derive-eval '(begin 1 2 ((lambda () 5)))) => 5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `let' tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (let-tests)
  (test (derive-eval '(let ((x 1) (y 2)) (+ x y))) => 3)
  (test (derive-eval '(let ((x 1) (y 2)) (+ x y ((lambda (x) (* 2 x)) 10)))) => 23)
  (test (derive-eval '(let ((x ((lambda () 5)))) x)) => 5)
  (test (derive-eval '(let ((f (lambda (x) (+ x x)))) (f 4))) => 8)
  (test (derive-eval '(let ((f (lambda (x) (+ x x))) (g (lambda (x y) (* x y))) (h (lambda () 2))) (f (g 3 (h))))) => 12))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; definition and function-definition tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (definition-tests)
  (test (derive-eval '(define x 2)) => 'ok)
  (test (derive-eval '(define (f x) (+ x x))) => 'ok)
  (test (derive-eval 'x) => 2)
  (test (derive-eval '(f x)) => 4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `if' tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (if-tests)
  (test (derive-eval '(if (> 3 2) 5 1)) => 5)
  (test (derive-eval '(if (< 3 2) 5 1)) => 1)
  (test (derive-eval '(if (> (+ 1 2) 2) (+ 1 2 3) 1)) => 6)
  (test (derive-eval '(define (f n) (if (= n 0) 1 (* n (f (- n 1)))))) => 'ok)
  (test (derive-eval '(f 5)) => 120))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `cond' tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cond-tests)
  (test (derive-eval '(cond ((> 3 2) 5) (else 1))) => 5)
  (test (derive-eval '(cond ((< 3 2) 5) (else 1))) => 1)
  (test (derive-eval '(let ((x 3)) (cond ((= x 1) 10) ((= x 2) 20) ((= x 3) 30) (else 100)))) => 30)
  (test (derive-eval '(let ((x 10)) (cond ((= x 1) 10) ((= x 2) 20) ((= x 3) 30) (else 100)))) => 100))


(define (while-tests)
  (test (derive-eval '(begin (define evegeny-n (box 0)) (while (> (unbox evegeny-n) 0) (set-box! evegeny-n (- (unbox evegeny-n) 1))) (unbox evegeny-n))) => 0))

(define (repeat-tests)
  (test (derive-eval '(begin (define evegeny-n (box 0)) (repeat (set-box! evegeny-n (- (unbox evegeny-n) 1)) (> (unbox evegeny-n) 0)) (unbox evegeny-n))) => -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Y-combinator tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (y-comb-tests)
  (test (derive-eval '(( (lambda (f) (f f))
                         (lambda (fact)
                           (lambda (n)
                             (if (= n 0)
                                 1
                                 (* n ((fact fact) (- n 1)))))))
                       6))
        => 720))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `Box' tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (Box-tests)
  (test (derive-eval '(let ((x (box 1))) (set-box! x (+ (unbox x) 5)) (unbox x))) => 6)
  (test (derive-eval '(let ((x (box 1))) ((lambda (x) (set-box! x 5) (unbox x)) x))) => 5)
  (test (derive-eval '(let ((x (box 1))) ((lambda (y) (set-box! x (* y y))) 4) (unbox x))) => 16)
  (test (derive-eval '(define f (let ((x (box 0)))
                                  (lambda () (set-box! x (+ (unbox x) 1)) (unbox x))))) => 'ok)
  (test (derive-eval '(f)) => 1)
  (test (derive-eval '(f)) => 2)
  (test (derive-eval '(begin (f) (f) (f) (f))) => 6)
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Invoking tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(run-tests
 (primitive-tests)
 (app-lambda-tests)
 (begin-tests)
 (let-tests)
 (definition-tests)
 (if-tests)
 (cond-tests)
 (Box-tests)
 (y-comb-tests)
 (while-tests)
 (repeat-tests))
