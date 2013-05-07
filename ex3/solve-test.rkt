#lang racket


(require "utils.rkt")
(require "solve.rkt")
(require "equation-adt.rkt")
(require "substitution-adt.rkt")
(require "type-expression-adt.rkt")
(require "ass2.rkt")
(require "utils.rkt")
(provide (all-defined-out))


(define (infer-type-tests) 
    (test (infer-type '(lambda (x) (+ (+ x 1) (+ x 1)))) => '(-> (* Number) Number))
    (test (infer-type '((lambda(x)(x 11)) (lambda(y) y))) => 'Number)
    (test (infer-type '(lambda (x) (+ x 1))) => '(-> (* Number) Number)))


(define (split-equation-tests) 
    (test (split-equation (make-equation-from-tes '(-> (* T_3) (-> (* T_3) T_1)) 
                                      '(-> (* T_3) (-> (* T_2) T_2)))) => '( ( (-> (* T_3) T_1) (-> (* T_2) T_2)) (T_3 T_3)))
    (test (split-equation (make-equation-from-tes '(-> (* T_3) Number) 
                                      '(-> (* Boolean) T_2))) => '((Number T_2) (T_3 Boolean)))
    (test (split-equation (make-equation-from-tes '(-> (* T_3 Boolean Number) (-> (* T_3) Number)) 
                                      '(-> (* T_3 T_2 Number) (-> (* Boolean) Number))))
          => '(((-> (* T_3) Number) (-> (* Boolean) Number)) (T_3 T_3) (Boolean T_2) (Number Number))))



(run-tests 
     (split-equation-tests)
     (infer-type-tests))


