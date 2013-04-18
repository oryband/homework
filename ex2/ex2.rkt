#lang racket
(provide (all-defined-out))

(define flatmap
  (lambda(f seq)
    (if (null? seq)
        '()
        (append (f (car seq))
                (flatmap f (cdr seq))))))

(define fresh-name
  (let((counter 0))
    (lambda name
      (if(null? name)
         (fresh-name 'var)
         (let((new-name (string->symbol (string-append (symbol->string (car name)) ":" (number->string counter)))))
           (set! counter (+ 1 counter))
           new-name)))))


(define (tagged-list? exp tag)
  (and (list? exp) (eq? (car exp) tag)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Variable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (variable? exp) (symbol? exp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Atomic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (atomic? exp)
  (or (number? exp) (boolean? exp) (variable? exp) (null? exp)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Quoted
;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (quoted? exp)
  (tagged-list? exp 'quote))


(define (text-of-quotation exp) (cadr exp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Lambda
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (lambda? exp) (tagged-list? exp 'lambda))


(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Your Code
;;;Signature: generate-type-vars (exp)
;;;Purpose: returns the sequence of sub-expressions with their type variables
;;;Type: T -> List[List[Symbol]
;;;Examples:
;;;(generate-type-vars '(lambda(x) x))=> '(((lambda (x) x) var_0) (x var_1))
;;;(generate-type-vars 'x)=> '((x var_0))
;;;(generate-type-vars 1)=>'((1 var_0))
