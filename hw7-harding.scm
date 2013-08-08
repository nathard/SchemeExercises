#lang racket
(require (lib "eopl.ss" "eopl"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;Problem 1 ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define the-lexical-spec
  ; Remove the empty list below and write your lexical spec instead.
  '((whitespace (whitespace) skip)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; Problem 2 ;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define the-grammar
  ; Remove the empty list below and write your grammar instead.
  '((expr ("true") true-exp)
    (expr ("false") false-exp)
    (expr ("&&" "(" (arbno expr) ")") and-exp)
    (expr ("||" "(" (arbno expr) ")") or-exp)
    (expr ("<" "(" expr expr ")") less-exp)
    (expr ("==" "(" expr expr ")") equal-exp)
    (expr (number) num-exp)
    (expr ("+" "(" (arbno expr) ")") add-exp)
    (expr ("-" "(" expr expr ")") sub-exp)
    (expr ("*" "(" (arbno expr) ")") mul-exp)
    (expr ("/" "(" expr expr ")") div-exp))
  )

(define scan&parse
  ; Remove the empty list and write the definition for scan&parse instead.
  (sllgen:make-string-parser the-lexical-spec the-grammar)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; Problem 3 ;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; print-infix : String -> String

(define print-helper
  (lambda (lon str)
    (if (null? (cdr lon)) 
        (print-infix (car lon))
        (string-append 
         (print-infix (car lon)) str (print-helper (cdr lon) str)))))

(define print-infix
  (lambda (exp) 
      (let ((x (if (string? exp) (scan&parse exp) exp)))
        (cases expr x
          (true-exp () "true")
          (false-exp () "false")
          (and-exp (lon) (string-append "(" (print-helper lon " && ") ")"))
          (or-exp (lon) (string-append "(" (print-helper lon " || ") ")"))
          (less-exp (rator rand) (string-append "("(print-infix rator) " < " (print-infix rand) ")"))
          (equal-exp (rator rand) (string-append "(" (print-infix rator) " == " (print-infix rand)")"))
          (num-exp (lon) (number->string lon))
          (add-exp (lon) (string-append "(" (print-helper lon " + ") ")"))
          (sub-exp (rator rand) (string-append "("(print-infix rator) " - " (print-infix rand) ")"))
          (mul-exp (lon) (string-append "(" (print-helper lon " * ") ")"))
          (div-exp (rator rand) (string-append "(" (print-infix rator) " / " (print-infix rand) ")"))))
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; Problem 4 ;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; compile-to-java : String -> String
(define compile-to-java
  ; Remove the empty list below and write your function implementation instead.
  (lambda (opt)
    (string-append 
     "class HW7_Q4_Solution{\n" 
     "\tpublic static void main(String[] args) {" 
     "\n\t\t System.out.println(" (print-infix opt) ");"
     "\n\t}"
     "\n}")
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; Problem 5 ;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;compute-cycles: String -> Number
(define compute-cycles
  ; Remove the empty list below and write your function implementation instead.
  (lambda (exp) 
    (let ((x (if (string? exp) (scan&parse exp) exp)))
      (cases expr x
        (true-exp () 0)
        (false-exp () 0)
        (and-exp (lon) (if (null? (cddr lon)) (+ 1 (compute-cycles (car lon)) (compute-cycles (cadr lon))) (+ 1 (compute-cycles (and-exp (cdr lon))))))
        (or-exp (lon) (if (null? (cddr lon)) (+ 1 (compute-cycles (car lon)) (compute-cycles (cadr lon))) (+ 1 (compute-cycles (or-exp (cdr lon))))))
        (less-exp (rator rand) (+ 1 (compute-cycles rator) (compute-cycles rand)))
        (equal-exp (rator rand) (+ 1 (compute-cycles rator) (compute-cycles rand)))
        (num-exp (lon) 0)
        (add-exp (lon) (if (null? (cddr lon)) (+ 2 (compute-cycles (car lon)) (compute-cycles (cadr lon))) (+ 2 (compute-cycles (add-exp (cdr lon))))))
        (sub-exp (rator rand) (+ 2 (compute-cycles rator) (compute-cycles rand)))
        (mul-exp (lon) (if (null? (cddr lon)) (+ 4 (compute-cycles (car lon)) (compute-cycles (cadr lon))) (+ 4 (compute-cycles (mul-exp (cdr lon))))))
        (div-exp (rator rand) (+ 4 (compute-cycles rator) (compute-cycles rand)))
       )
      )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; Problem 6 ;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Implement the "value" data type here.

(define-datatype value value?
  (Number (val integer?))
  (Bool (val boolean?)))

; Write your extractors and predicates here.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; Problem 7 ;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; value-of: exp-> value
(define value-of
  ; Remove the empty list below and write your function implementation instead.
  (lambda (exp)
    (cases expr exp
      (true-exp () #t)
      (false-exp () #f)
      (and-exp (lon)  (if (null? lon) #t (if (eq? (true-exp) (car lon)) (value-of (and-exp (cdr lon))) #f)))
      (or-exp (lon) (if (null? lon) #f (if (eq? (true-exp) (car lon)) #t (value-of (and-exp (cdr lon))))))
      (less-exp (rator rand) (< (value-of rator) (value-of rand)))
      (equal-exp (rator rand) (eq? (value-of rator) (value-of rand)))
      (num-exp (lon) lon)
      (add-exp (lon) (if (null? lon) 0 (+ (value-of (car lon)) (value-of (add-exp (cdr lon))))))
      (sub-exp (rator rand) (- (value-of rator) (value-of rand)))
      (mul-exp (lon) (if (null? lon) 1 (* (value-of (car lon)) (value-of (add-exp (cdr lon))))))
      (div-exp (rator rand) (/ (value-of rator) (value-of rand)))
      ))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; Problem 8 ;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; calculate: String-> value
(define calculate
  ; Remove the empty list below and write your function implementation instead.
  (lambda (lon)
    (value-of (scan&parse lon)))
  )




;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))
