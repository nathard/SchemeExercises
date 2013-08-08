#lang eopl

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; Problem 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Solve problem 1 here
(define-syntax begin342
  (syntax-rules ()
    ((begin342 e ...)
     ((lambda () e ...)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; Problem 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Solve problem 2 here

(define true342 (lambda (a b) a))
(define false342 (lambda (a b) b))

(define-syntax-rule
  (if342 x y z)
  (x y z))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; Problem 3 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Problem 3.a. As this is a non-code problem, please write answer
; as comments using semi-colons.

; (empty-stack) = !()!
; (push !e! !s!) = !n!, where !s + e!
; (pop !s!) = !n!, where !s - (car e)!
; (top !s!) = !car s!, s>=0
; (empty-stack? !s!) = {#t s=(), #f s!=()}

; Problem 3.b
(define is-constructor-empty-stack
  (lambda()
    ; Write #t if constructor, #f if observer
    #t
    ))

(define is-constructor-push
  (lambda()
    ; Write #t if constructor, #f if observer
    #t
    ))

(define is-constructor-pop
  (lambda()
    ; Write #t if constructor, #f if observer
    #t
    ))

(define is-constructor-top
  (lambda()
    ; Write #t if constructor, #f if observer
    #f
    ))

(define is-constructor-empty-stack?
  (lambda()
    ; Write #t if constructor, #f if observer
    #f
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; Problem 4 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Write your constructors/observers here.
; five procedures: empty-stack, push, pop, top, empty-stack?

(define empty-stack
	(lambda () '()))

(define push 
	(lambda (elem stk)
        (cons elem stk)))

(define pop
	(lambda (stk)
        (if (null? stk) (error "Stack empty")
        (cdr stk))))


(define top 
	(lambda (stk)
        (if (null? stk)
        (error "Stack empty")
        (car stk))))


(define empty-stack? 
	(lambda (stk) (null? stk)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; Problem 5 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Write your constructors/observers here.
; five procedures: empty-stack, push, pop, top, empty-stack?

(define empty-stack-v 
  (lambda () (make-vector 0)))

(define push-v 
  (lambda (vect elem) 
    (vector-append (vector elem) vect)))

(define top-v
  (lambda (vect)
    (vector-ref vect 0)))

(define empty-stack-v?
  (lambda (vect) 
    (eq? (vector-length vect) 0)))