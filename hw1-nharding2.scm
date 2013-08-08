#lang eopl

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; Problem 1.a ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Problem 1a
  (define problem1a-first
    (lambda ()
      ; Write your answer for ((8 + 3) * 4) here
      (* (+ 8 3) 4)
      ))
  
  (define problem1a-second
    (lambda ()
      ; Write your answer for ((8 * 3) / (2 + (4 * 3))) here
      (/ (* 8 3) (+ 2(* 4 3)))
      ))

  (define problem1a-third
    (lambda ()
      ; Write your answer for (((149 + 51) - (20 + (3 * 3))) * 2)  here
      (* (- (+ 149 51) (+ 20 (* 3 3))) 2)
      ))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; Problem 1.b ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ; Answer Problem 1 b here. (as this is not a coding question
  ; write answer in terms of comments, using semi-colons like this line)

  ; Algebraic formulas are translated in scheme using prefix notation
  ; e.g. (<operator> <arg1> <args*>)
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; Problem 1.c ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ; Answer Problem 1 c here.
  (define x 2)
  (define y 3)
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; Problem 1.d ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   (define problem1d
    (lambda ()
      ; Answer Problem 1 d here.
    (define (abs x y)
    (cond ((> x y) x)
          ((= x y) 0)
          ((< x y) y)))
		(abs x y)  
      
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; Problem 1.e ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   
   
   (define problem1e
    (lambda ()
      ; Answer Problem 1 e here.
    (define (abs x y)
    (cond ((< x y) x)
          ((= x y) 0)
          ((> x y) y)))
		(abs x y)  
    ))
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; Problem 1.f ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ; Answer Problem 1f here
   (equal? x y)
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; Problem 1.g ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   
  ; Answer Problem 1g here. (as this is not a coding question
  ; write answer in terms of comments, using semi-colons like this line)
   
  ; 'q can also be represented as (quote q) meaning do not evaluate q but quote the actual word q

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; Problem 1.h ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ; Answer Problem 1h here. (as this is not a coding question
  ; write answer in terms of comments, using semi-colons like this line)
  
  ; a) Strings are mutable and therefore you can change the sequences of characters it contains
  ; b) Symbols can be used like enumerated types such that you can quote keywords such as an integers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; Problem 1.i ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define problem1i-first
    (lambda ()
      ; Type your answer for (9 3 2) here
      (list 9 3 2)
      ))
  
  
  (define problem1i-second
    (lambda ()
      ; Type your answer for (student (id (number)) (major (department))) here
      (list 'student (list 'id (list 'number)) (list 'major (list 'department)))
      ))
    
  
  (define problem1i-third
    (lambda ()
      ; Type your answer for (((149 + 51) - (20 + (3 * 3))) * 2) here
      (list (list (list 149 '+ 51) '- (list 20 '+ (list 3 '* 3))) '* 2)
      ))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; Problem 1.j ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  
  ; Answer problem 1.j here. You can submit this problem in one of two ways:
  
  ; 1- write the figure here (as in the scheme file using "*", square brackets,
  ; and so on). OR
  ; 2- Scan the drawing and submit it as a separate file. Please make sure
  ; that the size of the file does not exceed 0.5 MB by any means and that
  ; the drawing is "clear".
  
  

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; Problem 1.k ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
   (define problem1k-first
    (lambda ()
      ; Type your answer for (9 3 2) here
      (cons 9 
            (cons 3 
                  (cons 2 '())))
      ))
  

  (define problem1k-second
    (lambda ()
      ; Type your answer for (student (id (number)) (major (department))) here
      (cons 'student 
            (cons (cons 'id (cons (cons 'number '()) '())) 
                  (cons (cons 'major (cons (cons 'department '()) '())) '())))
      ))
    
  
  (define problem1k-third
    (lambda ()
      ; Type your answer for (((149 + 51) - (20 + (3 * 3))) * 2) here
      (cons (cons 
             (cons 149 (cons '+ (cons 51 '()))) 
             (cons '- (cons 
                       (cons 20 (cons '+ (cons (cons 3 (cons '* (cons 3 '()))) '()))) '()))) 
            (cons '* (cons 2 '())))
      ))
   
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; Problem 1.l ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define lst (list 'a 'b 'c))
  
  
  (define problem1l-first
    (lambda ()
      ; Type your answer for (a b c d) here **corrected to match hw sheet
      (list (car lst) (car (cdr lst)) (car (cdr (cdr lst))) 'd)
      
      ))

  (define problem1l-second
    (lambda ()
      ; Type your answer for (a b d a b) here

      (list (car lst) (car (cdr lst)) 'd (car lst) (car (cdr lst)))
      ))
  
  (define problem1l-third
    (lambda ()
      ; Type your answer for (b c d) here

      (list (car (cdr lst)) (car (cdr (cdr lst))) 'd)
      ))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; Problem 1.m ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ; Answer problem 1.m here
  (define 1mlst (list 'a 'b 'c))
  1mlst
  (car 1mlst)
  1mlst
  1mlst
  (cdr 1mlst)
  1mlst
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; Problem 1.n ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ; Answer problem 1.n here. (as this is not a coding question
  ; write answer in terms of comments, using semi-colons like this line)

  ;vectors are mutable there contents can be changed, lists must be reconstructed
  ;vectors in java must be of fixed value.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; Problem 1.o ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ; Problem 1.o
  
     (define student '((id (number)) (major (department))))


    
    (define problem1o-first
      (lambda ()
        ; Type your answer for (cdr (car student)) here in terms of string. That is, whatever your answer is,
        ; put in between quotes. For instance, if the answer to the question is (street), type "(street)".
        "((number))"
        ))
    
    (define problem1o-second
      (lambda ()
        ; Type your answer for (cdar student) here in terms of string. That is, whatever your answer is,
        ; put in between quotes. For instance, if the answer to the question is (street), type "(street)".
        "((number))"
        ))

    (define problem1o-third
      (lambda ()
        ; Type your answer for (cadr student) here in terms of string. 
        "(major (department))"
        ))
    
    (define problem1o-fourth
      (lambda ()
        ; Type your answer for (cadar student) here in terms of string. 
        "(number)"
        ))

   (define problem1o-fifth
      (lambda ()
        ; Type your answer for (cdadar student) here in terms of string. 
        "()"
        ))

   (define problem1o-sixth
      (lambda ()
        ; Type your answer for (car (cadar student)) here in terms of string.
        "number"
        ))

  (define problem1o-seventh
      (lambda ()
        ; Type your answer for (cons 'a (cons 'b '())) here in terms of string. 
        "(a b)"
        ))
    
   (define problem1o-eighth
      (lambda ()
        ; Type your answer for (cons (cons (cons 'a (cons 'b '())) '()) '()) here in terms of string. 
        "(((a b)))"
        ))
        
  (define problem1o-nineth
      (lambda ()
        ; Type your answer for (cons 'a (cons (cons 'b (cons 'c '())) '())) here in terms of string. 
        "(a (b c))"
        ))
    
      
  (define problem1o-tenth
      (lambda ()
        ; Type your answer for (cons (cons 'a (cons 'b (cons (cons 'c '()) '()))) 'student) here in terms of string. 
        "((a b (c)) . student)"
        ))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; Problem 1.p ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ; Given
  (define m '(a b c))
  (define n '(c d e))
  (define z (cons m n))
  
  (define problem1p-first
    (lambda()
      ; Type your answer for z here in terms of string. For instance, if answer is (a b), write "(a b)"
      "((a b c) c d e)"
      ))
  
  (define problem1p-second
    (lambda()
      ; Type your answer for (eq? z (cons m n)) in terms of string. For instance, if the answer is #t, type "#t".
      "#f"
      ))
  
  
  (define problem1p-third
    (lambda()
      ; Type your answer for (equal? z (cons m n)) in terms of string. For instance, if the answer is #t, type "#t".
      "#t"
      ))
  
  
  (define problem1p-fourth
    (lambda()
      ; Type your answer for (eq? (car z) m) in terms of string. For instance, if the answer is #t, type "#t".
      "#t"
      ))
  
  
  (define problem1p-fifth
    (lambda()
      ; Type your answer for "(equal? (car z) n)" in terms of string. For instance, if the answer is #t, type "#t".
      "#f"
      ))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; Problem 1.q ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define vec1 (vector 1 2 3))
  (define vec2 (vector 1 2 3))
  
  (define problem1q-first
    (lambda()
      ; Type answer for vec1 in terms of string.
      "#(1 2 3)"
    ))
  
 (define problem1q-second
   (lambda()
      ; (eq? vec1 vec2) also in terms of string. Same applies for all problem1q-*
     "#f"
   ))
  
 (define problem1q-third
   (lambda()
      ; (equal? vec1 vec2)
     "#t"
   ))
 
 (vector-set! vec1 2 9)
 
 
 (define problem1q-fourth
   (lambda()
      ; vec1
     "#(1 2 9)"
   ))

 (define problem1q-fifth
   (lambda()
      ; vec2
     "#(1 2 3)"
   ))

 (define problem1q-sixth
   (lambda()
      ;(equal? vec1 vec2)
     "#f"
   ))
 
 (define problem1q-seventh
   (lambda()
      ; (eq? vec1 vec2)
     "#f"
   ))

 (define vec3 vec2)
 
 (define problem1q-eighth
   (lambda()
     ;(equal? vec3 vec2)
     "#t"
   ))
 
  (define problem1q-nineth
    (lambda()
    ; (eq? vec3 vec2)
      "#t"
    ))
 
 (vector-set! vec2 2 9)
   
 (define problem1q-tenth
   (lambda()
      ;  vec2
     "#(1 2 9)"
   ))


 (define problem1q-eleventh
   (lambda()
      ;vec 3
     "#(1 2 9)"
   ))

 (define problem1q-twelve
   (lambda()
       ; (equal? vec3 vec2)
     "#t"
   ))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; Problem 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ; Answer Problem 2 here. Write answer in terms of comments
  ; as this is a non-code question.

  ; equal? compares the printed output.
  ; eq? compares the equivalence of the 2 objects (like eqv)
  ; so for instance vec1 and vec2 are not equal because they are not the same
  ; object even though they contain the same values. 
  ; whereas vec2 and vec3 are eq because they are defined as the same object.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; Problem 3 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ; Solve Problem 3 here
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; Problem 4 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  ; Solve Problem 4 here


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; Problem 5 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ; Solve Problem 5 here
  ; couldn't figure out how to place condition on 4 words only.
(define (delete-third lst)
  (list (car lst) (car (cdr lst)) (cadddr lst)))
