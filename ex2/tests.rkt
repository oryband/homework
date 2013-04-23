#lang racket
(require "utils.rkt")
(include "ex2.rkt")



(define (element-of-set? exp-var-pair exp-var-set)
  (cond ((null? exp-var-set) #f)
        ((and (equal? (car exp-var-pair)
                      (caar exp-var-set))
              (symbol?(cadr exp-var-pair))) #t)

        (else (element-of-set? exp-var-pair (cdr exp-var-set)))))


(define (equal-results?  expected-set got-set)
  (if (and (null? expected-set) (null? got-set))
      #t
      (if (not (eq? (length expected-set)  (length got-set)))
          #f
          (letrec ((check-equality (lambda(set)
                                     (cond ((null? set) #t)
                                           ((element-of-set? (car set) got-set)
                                            (check-equality (cdr set)))
                                           (else  #f)))))
            (check-equality expected-set)))))


(define test-generate-type-vars
  (lambda()
    (run-tests
     (test (let ((res (generate-type-vars 1) )) (equal-results? '((1 T_0)) res)) => #t)
     (test (let ((res (generate-type-vars 'x) )) (equal-results? '((x T_0)) res)) => #t)
     (test (let ((res (generate-type-vars '+) )) (equal-results? '((+ T_1)) res)) => #t)
     (test (let ((res (generate-type-vars #t) )) (equal-results? '((#t T_0)) res)) => #t)

     (test (let
               ((res (generate-type-vars '(lambda(x) x) )))
                (equal-results? '(((lambda (x) x) T_0) (x T_1)) res))  => #t)

     (test (let
               ((res (generate-type-vars '(f x y))))
             (equal-results? '(((f x y) T_0)
                               (f T_1)
                               (y T_3)
                               (x T_2))
                             res))
           => #t)
     (test (let
               ((res (generate-type-vars  '(lambda(x y z) (+ x y z 5)) )))
                (equal-results? '(((lambda (x y z) (+ x y z 5)) T_0)
                                  ((+ x y z 5) T_1)
                                  (+ T_2)
                                  (5 T_6)
                                  (z T_5)
                                  (y T_4)
                                  (x T_3))
                                res))
           => #t)


     (test (let ((res (generate-type-vars '(((lambda(x y) (lambda(z) (+ y 1) (z x))) 9 10) sqrt))))
             (equal-results? '(
                               ((((lambda (x y) (lambda (z) (+ y 1) (z x))) 9 10) sqrt) T_0)
                               (((lambda (x y) (lambda (z) (+ y 1) (z x))) 9 10) T_1)
                               ((lambda (x y) (lambda (z) (+ y 1) (z x))) T_2)
                               ((lambda (z) (+ y 1) (z x)) T_3)
                               ((z x) T_8)
                               (z T_9)
                               (x T_10)
                               ((+ y 1) T_4)
                               (+ T_5)
                               (1 T_7)
                               (y T_6)
                               (10 T_12)
                               (9 T_11)
                               (sqrt T_13))
                             res)) => #t))))

(test-generate-type-vars)
