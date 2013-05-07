#lang racket

(require "asp.rkt" "type-expression-adt.rkt" "ass2.rkt")
(provide (all-defined-out))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Equation ADT
; An equation is implemented as a list of two type expressions.

;Signature: make-equation-from tes(te)
;Type: 
(define make-equation-from-tes
  (lambda (type-expr-l type-expr-r) 
    .....))

;Signature: make-empty-equation()
;Type: 
(define make-empty-equation
  (lambda () 
    .....))

  
  
  
  
  
  

;Signature: get-left(eq)
;Type: 
(define get-left
  (lambda (eq) 
    .....
   ))

;Signature: get-right(eq)
;Type: 
(define get-right
  (lambda (eq) 
    ....
   ))

;Signature: equation?(eq)
;Type: 
(define equation?
  (lambda (eq) 
    ))


;Signature: empty-equation?(eq)
;Type: 
(define empty-equation?
  (lambda (eq) .....))




  
  
  
;Signature: get-var-of-expr(expr-tvars-list expr)
;Type: [LIST(LIST(LIST union Symbol * Symbol)) * LIST union Symbol -> Symbol]
; Purpose: Find the type of a Scheme expression in a list of pairs of a Scheme-expression and a type vars
;Tests: (get-var-of-expr '(((lambda (x) (+ x 1)) T_0) ((+ x 1) T_1) (1 T_4) (x T_3) (+ T_2)) '+)  ==> T_2 
(define get-var-of-expr
  (lambda (expr-tvars-list expr)
    (let ((expr-pair (assoc expr expr-tvars-list)))
      (if (and expr-pair (not (null? expr-pair)))
          (cadr expr-pair)
          void))
    ))



(define binary-numeric-primitive-type (list (make-proc-te (make-tuple-te (list 'Number 'Number)) 'Number)))
(define binary-logical-primitive-type (list (make-proc-te (make-tuple-te (list 'Number 'Number)) 'Boolean)))
(define binary-primitive-types (list (list '+ binary-numeric-primitive-type) (list '- binary-numeric-primitive-type) (list '* binary-numeric-primitive-type)
                                     (list '> binary-logical-primitive-type)))


;Signature: primitive-procedure?(se)
;Type: (LIST union Symbol -> Boolean]
;Purpose:
;Example: (primitive-procedure? '+)  ==> #t 
(define (primitive-procedure? se) 
  (if (or (not (symbol? se))
          (null? (filter (lambda(x) (and (list? x)(eq? (car x) se))) binary-primitive-types)))
         #f
         #t
     )
  )


;Signature: get-primitive-type(se)
;Type: (LIST union Symbol -> Boolean]
;Purpose: 
;Example: (get-primitive-type '+)  ==> '(-> (* 'Number 'Number) 'Number)]
(define (get-primitive-type se)
  (let ((filtered (filter (lambda(x) (and (list? x)(eq? (car x) se))) binary-primitive-types)))
    (if (null? filtered) (list)
        (caadar filtered)
    )
  ))


;Signature:  make-equations(Scheme-expression,expression-type-vars-list)
;Type:
(define make-equations
  (lambda (expr expr-tvars-list)    
      .....
      )
    )

  
  
  
  ;Signature: get-first-equation(equations)
;Type: 
(define get-first-equation
  (lambda (equations) (car equations)))

;Signature: get-rest-equations(equations)
;Type: 
(define get-rest-equations
  (lambda (equations) (cdr equations)))