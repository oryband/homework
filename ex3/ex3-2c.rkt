#lang racket

(require "asp.rkt")
(provide (all-defined-out))

;;;;;;;;;;;  Implementation of the Type-Expression ADT ;;;;;;;;;;;;;;;;;;

;;; Type grammar:
; Type -> Atomic | Composite | Type-variable
; Atomic -> ‘Number’ | ‘Boolean’
; Composite -> Procedure | Tuple
; Procedure -> ‘[‘Tuple’ ->’ Type’]’
; Tuple -> (Type ‘*’) Type | ‘Empty’
; Type-variable -> a Symbol

; Constructors:
;Number, Boolean


(define make-proc-te
  (lambda (tuple-te te)
    ....))

;Signature: make-tuple-te(te-list)
;Type: [LIST -> lIST]
;Purpose: get tuple
;Tests: (make-tuple-te (list 'Number (make-proc-te (make-tuple-te (list 'Number)) 'T1)))
;                                                           ==> (* Number (-> (* Number) T1))
(define make-tuple-te
  (lambda (te-list)
    (cons '* te-list)))

; Getters:


(define get-constructor
  (lambda (te)
    ...))

;Signature: tuple-components(te)
;Type: [LIST union Symbol -> LIST]
;Purpose: get tuple components
;Tests: (tuple-components (make-tuple-te (list 'Number 'Number))) ==> (Number Number)
(define tuple-components
  (lambda (te)
    (if (tuple? te)
        (cdr te)
        (list))))

;Signature: tuple-length(te)
;Type: [LIST union Symbol -> Number]
;Purpose: get tuple length
;Tests: (tuple-length (make-tuple-te (list 'Number 'Number))) ==> 2
(define tuple-length
  (lambda (te)
    (if (tuple? te)
        (length (tuple-components te))
        (list))))


(define proc-parameter-tuple-tes
  (lambda (te)
    ...))


(define proc-parameter-tes
  (lambda (te)
    ...))


(define proc-return-te
  (lambda (te)
    ...))


;Signature: equal-atomic-te?(te1 te2)
;Type: [LIST union Symbol * LIST union Symbol -> Boolean]
;Purpose: are to type exressions equal
;Tests: (equal-atomic-te? 'Number 'Number) ==> #t
(define equal-atomic-te?
  (lambda (te1 te2)
    (and (symbol? te1) (symbol? te2) (eq? te1 te2))))

;;;;;;;;;;;;;;;
;identifiers:

;Signature: type-expr?(te)
;Type: [LIST union Symbol -> Boolean]
;Purpose: is type expresion
;Tests: (type-expr?(make-proc-te (make-tuple-te (list 'Number)) 'T1)) ==> #t
(define type-expr?
  (lambda (te)
    (or (atomic? te)(variable? te) (composite? te))))

;Signature: atomic?(te)
;Type: [LIST union Symbol -> Boolean ]
;Purpose: is atomic
;Tests: (atomic? (make-proc-te (make-tuple-te (list 'Number)) 'T1)) ==> #f
(define atomic?
  (lambda (te)
    (or (eq? te 'Number)(eq? te 'Boolean))))

;Signature: composite?(te)
;Type: [LIST union Symbol -> Boolean
;Purpose: is composite
;Tests: (composite? (make-proc-te (make-tuple-te (list 'Number)) 'T1)) ==> #t
(define composite?
  (lambda (te) (or (procedure? te) (tuple? te))))

;Signature: tuple?(te)
;Type: [LIST union Symbol -> Boolean ]
;Purpose: is tuple
;Tests: (tuple? (make-proc-te (make-tuple-te (list 'Number)) 'T1)) ==> #f
(define tuple?
  (lambda (te)
    (and (list? te)(eq? (car te) '*)) ))


(define procedure?
  (lambda (te)
    ... ))

;Signature: variable?(te)
;Type: [LIST union Symbol -> Boolean
;Purpose: is variable
;Tests: (variable? (make-proc-te (make-tuple-te (list 'Number)) 'T1)) ==> #t
(define variable?
  (lambda (te)
    (and (not (atomic? te))(symbol? te))
   ))



