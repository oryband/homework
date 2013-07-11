#lang racket

(provide (all-defined-out))

; Signature: make-mbtree(val)
; Type: [T -> Mbtree]
; Purpose: Costructing a mutable binary tree
; Tests: 
(define make-mbtree
  (lambda(val)
    (let ((value (box val))
          (left (box 'null))
          (right (box 'null)))
    (cons 'mbtree (lambda (action)
                    (cond ((eq? action 'get-left) (unbox left))
                          ((eq? action 'get-right) (unbox right))
                          ((eq? action 'get-value) (unbox value))
                          ((eq? action 'set-left!) (lambda (newleft)(set-box! left newleft)))
                          ((eq? action 'set-right!) (lambda (newright)(set-box! right newright)))
                          ((eq? action 'set-value!) (lambda (newvalue)(set-box! value newvalue)))
                          (else (error 'unknown action))))))))


; Signature: make-empty-mbtree()
; Type: [Empty -> Mbtree]
; Purpose: Constructing an empty mbtree
; Tests: (make-empty-mbtree) ==> 'null
(define make-empty-mbtree
  (lambda() (make-mbtree 'null)))

; Signature: get-left()
; Type: [Mbtree -> Mbtree]
; Purpose:returns the left child of a the non-empty node tree, for an empty tree, t, : get-left(t)='null 
; Tests: (get-left (make-mbtree 1)) ==> 'null
(define get-left
  (lambda(mbtree) 
    ((cdr mbtree) 'get-left)))

; Signature: get-right()
; Type: [Mbtree -> Mbtree]
; Purpose:returns the right child of a the non-empty node tree, for an empty tree, t, : get-right(t)='null 
; Tests: (get-right (make-mbtree 1)) ==> 'null
(define get-right
 (lambda(mbtree) 
  ((cdr mbtree) 'get-right)))

; Signature: get-value()
; Type: [Mbtree -> T]
; Purpose:returns the value of a the non-empty node tree, for an empty tree, t, : get-value(t)='null 
; Tests: (get-value (make-mbtree 1)) ==> 1
(define get-value
 (lambda(mbtree) 
     ((cdr mbtree) 'get-value)))

; Signature: set-left!(mbtree,child)
; Type: [Mbtree*Mbtree -> Void]
; Purpose: sets left child of the tree mbtree to be child
; Tests: (set-left! (make-mbtree 1) (make-mbtree 2))
(define set-left!
  (lambda(mbtree child) 
    (((cdr mbtree) 'set-left!) child)))


; Signature: set-right!(mbtree,child)
; Type: [Mbtree*Mbtree -> Void]
; Purpose: sets right child of the tree mbtree to be child
; Tests: (set-right! (make-mbtree 1) (make-mbtree 2))
(define set-right!
  (lambda(mbtree child) 
     (((cdr mbtree) 'set-right!) child)))


; Signature:set-value!(mbtree,val)
; Type: [Mbtree*T -> Void]
; Purpose: sets value of the node tree mbtree to be val
; Tests: (set-right! (make-mbtree 1) 2)
(define set-value!
  (lambda(mbtree val) 
    (((cdr mbtree) 'set-value!) val)))

; Signature:empty-mbtree?(mbtree)
; Type: [Mbtree -> Boolean]
; Purpose: Check if the tree is empty tree
; Tests: (empty-mbtree? (make-empty-mbtree) => #t)
(define empty-mbtree?
  (lambda(mbtree)
    (and (mbtree? mbtree) (eq? (get-left mbtree) 'null) (eq? (get-right mbtree) 'null) (eq? (get-value mbtree) 'null))))

; Signature:mbtree?(mbtree)
; Type: [Mbtree -> Boolean]
; Purpose: Check if the parameter is Mbtree 
; Tests: (mbtree? (make-empty-mbtree) => #t)
(define mbtree?
  (lambda(mbtree) 
   (eq? (car mbtree) 'mbtree)))

;Signature: set-leftmost-occurrence!(mbtree, old, new)
;Type: [Mbtree * T1 * T2 -> Void]
;Purpose: Set the left most occurrence of “old” to “new”
;returning void in any case.
;Tests: 
;Post-condition:  tree = tree@pre with left most occurrence 
;                 of “old” set to “new”.
(define set-leftmost-occurrence!
  (lambda(tree old new) 
    (helper tree old new (box #f))))
(define helper (lambda (tree old new found)
      (begin (if (and (not(unbox found)) (not(eq? (get-left tree)'null))) 
                 (helper (get-left tree) old new found)
                 (unbox found))
             (if (and (not(unbox found)) (eq? (get-value tree) old))
                  (begin (set-box! found #t) (set-value! tree new))
                 (unbox found))
             (if (and (not(unbox found)) (not(eq? (get-right tree)'null)))
                 (helper (get-right tree) old new found)
                 (unbox found)))))
