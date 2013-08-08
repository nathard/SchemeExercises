#lang eopl

; the lang is eopl because it allows "cond" to have
; "else" clause.

;;;;;;;;;;;;;;;;;;;;
;Problem 1;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;


;Problem 1.a solve as comments

; num-exp, add-exp, sub-exp, mul-exp, div-exp

;Problem 1.b solve as comments

; 5 constructors

;Problem 1.c solve as comments

; 5 predicates

;Problem 1.d solve as comments

; 9 extractors

;Problem 1.e solve as comments

; Constructors:
; num-exp: Number -> arith-exp
; add-exp: arith-exp * arith-exp -> arith-exp
; sub-exp: arith-exp * arith-exp -> arith-exp
; mul-exp: arith-exp * arith-exp -> arith-exp
; div-exp: arith-exp * arith-exp -> arith-exp

; Predicates:
; num-exp?: arith-exp -> bool
; add-exp?: arith-exp -> bool
; sub-exp?: arith-exp -> bool
; mul-exp?: arith-exp -> bool
; div-exp?: arith-exp -> bool

; Constructors:
; num-exp->num: arith-exp -> num
; add-exp->left: arith-exp -> arith-exp
; add-exp->right: arith-exp -> arith-exp
; sub-exp->left: arith-exp -> arith-exp
; sub-exp->right: arith-exp -> arith-exp
; mul-exp->left: arith-exp -> arith-exp
; mul-exp->left: arith-exp -> arith-exp
; div-exp-left: arith-exp -> arith-exp
; div-exp-right: arith-exp -> arith-exp

;Problem 1.f
 ; Constructors
 (define num-exp (lambda (num) (list 'num-exp num)))
 (define add-exp (lambda (left right) (list 'add-exp left right)))
 (define sub-exp (lambda (left right) (list 'sub-exp left right)))
 (define mul-exp (lambda (left right) (list 'mul-exp left right)))
 (define div-exp (lambda (left right) (list 'div-exp left right)))
  
 ; Predicates
 (define num-exp? (lambda (a) (and (pair? a) (eq? (car a) 'num-exp))))
 (define add-exp? (lambda (a) (and (pair? a) (eq? (car a) 'add-exp))))
 (define sub-exp? (lambda (a) (and (pair? a) (eq? (car a) 'sub-exp))))
 (define mul-exp? (lambda (a) (and (pair? a) (eq? (car a) 'mul-exp))))
 (define div-exp? (lambda (a) (and (pair? a) (eq? (car a) 'div-exp))))
 
 ;Extractors
 (define num-exp->num  (lambda (a) (cadr a)))
 (define add-exp->left  (lambda (a) (cadr a)))
 (define add-exp->right  (lambda (a) (caddr a)))
 (define sub-exp->left  (lambda (a) (cadr a)))
 (define sub-exp->right  (lambda (a) (caddr a)))
 (define mul-exp->left  (lambda (a) (cadr a)))
 (define mul-exp->right  (lambda (a) (caddr a)))
 (define div-exp->left  (lambda (a) (cadr a)))
 (define div-exp->right  (lambda (a) (caddr a)))

;Problem 1.g

; Constructors
(define num-exp-p
  (lambda (num)
    (lambda (op)
      (cond ((eq? op 'eval) num)
      ((eq? op 'type) 'number)
        (else "Invalid operator")))))

(define add-exp-p
  (lambda (rator rand)
    (lambda (op)
      (cond ((eq? op 'eval) (+ (rator 'eval) (rand 'eval)))
      ((eq? op 'type) 'add)
      ((eq? op 'left) rator)
      ((eq? op 'right) rand)
        (else "Invalid operator")))))

(define sub-exp-p
  (lambda (rator rand)
    (lambda (op)
      (cond ((eq? op 'eval) (- (rator 'eval) (rand 'eval)))
      ((eq? op 'type) 'sub)
      ((eq? op 'left) rator)
      ((eq? op 'right) rand)
        (else "Invalid operator")))))

(define mul-exp-p
  (lambda (rator rand)
    (lambda (op)
      (cond ((eq? op 'eval) (* (rator 'eval) (rand 'eval)))
      ((eq? op 'type) 'mul)
      ((eq? op 'left) rator)
      ((eq? op 'right) rand)
        (else "Invalid operator")))))

(define div-exp-p 
  (lambda (rator rand)
    (lambda (op)
      (cond
      ((eq? op 'eval) (/ (rator 'eval) (rand 'eval)))
      ((eq? op 'type) 'div)
      ((eq? op 'left) rator)
      ((eq? op 'right) rand)
        (else "Invalid operator")))))


; predicates

(define num-exp-p? (lambda (a) (eq? 'number (a 'type))))
(define add-exp-p? (lambda (a) (eq? 'add (a 'type))))
(define sub-exp-p? (lambda (a) (eq? 'sub (a 'type))))
(define mul-exp-p? (lambda (a) (eq? 'mul (a 'type))))
(define div-exp-p? (lambda (a) (eq? 'div (a 'type))))

; extractors

(define num-exp-p->var (lambda (a) (a 'eval)))
(define add-exp-p->left (lambda (a) (if (add-exp-p? a) (a 'left) "error")))
(define add-exp-p->right (lambda (a) (if (add-exp-p? a) (a 'right) "error")))
(define sub-exp-p->left (lambda (a) (if (sub-exp-p? a) (a 'left) "error")))
(define sub-exp-p->right (lambda (a) (if (sub-exp-p? a) (a 'right) "error")))
(define mul-exp-p->left (lambda (a) (if (mul-exp-p? a) (a 'left) "error")))
(define mul-exp-p->right (lambda (a) (if (mul-exp-p? a) (a 'right) "error")))
(define div-exp-p->left (lambda (a) (if (div-exp-p? a) (a 'left) "error")))
(define div-exp-p->right (lambda (a) (if (div-exp-p? a) (a 'right) "error")))

;Problem 1.h

(define eval-exp
    (lambda (exp)
      (cond
        ((num-exp? exp) (num-exp->num exp))
        ((add-exp? exp) (+ (eval-exp (add-exp->left exp)) (eval-exp (add-exp->right exp))))
        ((sub-exp? exp) (- (eval-exp (sub-exp->left exp)) (eval-exp (sub-exp->right exp))))
        ((mul-exp? exp) (* (eval-exp (mul-exp->left exp)) (eval-exp (mul-exp->right exp))))
        ((div-exp? exp) (/ (eval-exp (div-exp->left exp)) (eval-exp (div-exp->right exp))))
        (else (display "Incorrect try again")))))

;Problem 1.i

(define-datatype arith-exp arith-exp?
  (num-exp-d (num number?))
  (add-exp-d (left arith-exp?) (right arith-exp?))
  (sub-exp-d (left arith-exp?) (right arith-exp?))
  (mul-exp-d (left arith-exp?) (right arith-exp?))
  (div-exp-d (left arith-exp?) (right arith-exp?))
  )
 
;Problem 1.j

(define eval-exp-d
  (lambda (exp)
    (cases arith-exp exp
      (num-exp-d (num) num)
      (add-exp-d (left right) (+ (eval-exp-d left) (eval-exp-d right)))
      (sub-exp-d (left right) (- (eval-exp-d left) (eval-exp-d right)))
      (mul-exp-d (left right) (* (eval-exp-d left) (eval-exp-d right)))
      (div-exp-d (left right) (/ (eval-exp-d left) (eval-exp-d right))))))


;;;;;;;;;;;;;;;;;;;;
;Problem 2;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;


;Problem 2.a solve as comments

; filled-triangle, trema, vertex

;Problem 2.b solve as comments

; 3 constructors

;Problem 2.c solve as comments

; 3 predicates

;Problem 2.d solve as comments

; 8 extractors

;Problem 2.e solve as comments

; constructors

; filled-triangle: vertex * vertex * vertex -> st
; trema: st * st * st -> st
; Vertex: Number * Number -> vertex

; predicates

; filled-triangle?: st -> bool
; trema?: st -> bool
; vertex?: vertex -> bool

; extractors

; filled-triangle->top: st -> vertex
; filled-triangle->left: st -> vertex
; filled-triangle->right: st -> vertex
; trema->top: st -> st
; trema->left: st -> st
; trema->right: st -> st
; Vertex->left: vertex -> number
; Vertex->right: vertex -> number

;Problem 2.f

; constructors

(define filled-triangle
  (lambda (v1 v2 v3)
    (if (and (vertex? v1) (vertex? v2) (vertex? v3)) (list 'filled-triangle v1 v2 v3) 
        '(error))))

(define trema
  (lambda (st1 st2 st3)
    (if (or (and (filled-triangle? st1) (filled-triangle? st2) (filled-triangle? st3)) 
    (and (trema? st1) (trema? st2) (trema? st3))) 
        (list 'trema st1 st2 st3) '(error))))

(define vertex
  (lambda (n1 n2)
    (if (and (number? n1) (number? n2)) (list 'vertex n1 n2) 
        '(error))))

; predicates

(define filled-triangle? (lambda (st) (eq? (car st) 'filled-triangle)))
(define trema? (lambda (st) (eq? (car st) 'trema)))
(define vertex? (lambda (vx) (eq? (car vx) 'vertex)))

; extractors

(define filled-triangle->top (lambda (st) (cadr st)))
(define filled-triangle->left (lambda (st) (caddr st)))
(define filled-triangle->right (lambda (st) (cadddr st)))
(define trema->top (lambda (st) (cadr st)))
(define trema->left (lambda (st) (caddr st)))
(define trema->right (lambda (st) (cadddr st)))
(define vertex-left (lambda (vx) (cadr vx)))
(define vertex-right (lambda (vx) (caddr vx)))

;Problem 2.g

; constructors

(define filled-triangle-p
  (lambda (vx1 vx2 vx3)
    (if (and (vertex-p? vx1) (vertex-p? vx2) (vertex-p? vx3))
      (lambda (op)
        (cond
          ((eq? op 'type) 'filled-triangle)
          ((eq? op 'top) vx1)
          ((eq? op 'left) vx2)
          ((eq? op 'right) vx3)
            (else "error")))
              (lambda (op) #f))))

(define trema-p
  (lambda (st1 st2 st3)
    (if (or (and (filled-triangle-p? st1) (filled-triangle-p? st2) (filled-triangle-p? st3))
    (and (trema-p? st1) (trema-p? st2) (trema-p? st3)))
      (lambda (op)
        (cond ((eq? op 'type) 'trema)
        ((eq? op 'top) st1)
        ((eq? op 'left) st2)
        ((eq? op 'right) st3)
          (else "error")))
            (lambda (op) #f))))

(define vertex-p
  (lambda (n1 n2)
    (if (and (number? n1) (number? n2))
      (lambda (op)
        (cond ((eq? op 'type) 'vertex)
        ((eq? op 'n1) n1)
        ((eq? op 'n2) n2)
          (else "error")))
            (lambda (op) #f))))

; predicates

(define filled-triangle-p? (lambda (st) (eq? 'filled-triangle (st 'type))))
(define trema-p? (lambda (st) (eq? 'trema (st 'type))))
(define vertex-p? (lambda (vx) (eq? 'vertex (vx 'type))))
; extractors

(define filled-triangle-p->top (lambda (st) (st 'top)))
(define filled-triangle-p->left (lambda (st) (st 'left)))
(define filled-triangle-p->right (lambda (st) (st 'right)))
(define trema-p->top (lambda (st) (st 'top)))
(define trema-p->left (lambda (st) (st 'left)))
(define trema-p->right (lambda (st) (st 'right)))
(define vertex-p->left (lambda (vx) (vx 'x)))
(define vertex-p->right (lambda (vx) (vx 'y)))

;Problem 2.h

(define-datatype st st?
  (filled-triangle-d (top st?) (left st?) (right st?) )
  (trema-d (top st?) (left st?) (right st?))
  )

(define-datatype vertex-d vertex-d?
  (Vertex-d (left number?) (right number?))
  )