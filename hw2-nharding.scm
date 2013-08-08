#lang eopl

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; Problem 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Problem 1
  (define between
    ;answer here (lambda ...
    (lambda (start)
      (lambda (end)
         (lambda (num) 
            (if (and (<= num end) (>= num start)) #t #f)
           )))
    )
  
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; Problem 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define teen
    ;answer here (lambda ...
    (lambda (age)
          (((between 13) 19) age))
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; Problem 3 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define occurs?
    ; answer here
    '()
    )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; Problem 4 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (problem4)
    ; Write #t if answer is yes or #f if answer is no
    #t
    )
    
    ; Write explanation in English (using semi-colons to comment)
  
    ; Increment and decrement operations can be desugared using the expressions (n + 1) and (n - 1) recursively 
  
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; Problem 5 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (problem5-a)
    ; Write #t (which means correct) or #f (which means incorrect).
    #t
    )
    
    ; Write explanation in English in case of #f (using semi-colons to comment)

  (define (problem5-b)
    ; Write #t (which means correct) or #f (which means incorrect).
    #f
    )
    
    ; Write explanation in English in case of #f (using semi-colons to comment)
    ; Recursion is only processing the beginning of the list.
  
  (define (problem5-c)
    ; Answer here
    #f
    )
    
    ; Explanation in case of #f
    ; Beginning of list is being ignored by procedure
  
  (define (problem5-d)
    ; Answer here
    #t
    )
    
    ; Explanation in case of #f
    
   (define (problem5-e)
     ; Answer here
    #t
     )
    
    ; Explanation in case of #f
    
    (define (problem5-f)
      ; Answer here
      #f
      )
    
    ; Explanation in case of #f
    ; Missing checks for null list
    
    


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; Problem 6 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (define (problem6-a)
      ; Answer #t if correct or #f if not
      #f
      )   
    
    ; Explanation in case of #f (write as comments using semi-colons like this line)
    ; (begin (cdr words) truncates the list before it can check the first word
    
    (define (problem6-b)
      ; Answer #t or #f
      #t
      )
    ; Explanation
    
      
    (define (problem6-c)
      ; Answer #t or #f
      #f
      )
    
    ; Explanation
    ; recursion appears 1 too many times with incorrect input
    
 
    (define (problem6-d)
      ; Answer #t or #f
      #t
      )
    ; Explanation


     (define (problem6-e)
      ; Answer #t or #f
       #t
       )
    ; Explanation

     
    (define (problem6-f)
      ; Answer #t or #f
      #f
      )
    ; Explanation
    ;No null condition
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; Problem 7 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  
  (define frequency
    ; answer here
    (lambda (lst elem)
      (cond
        ((null? lst) 0)
        ((eq? (car lst) elem) (+ (frequency (cdr lst) elem) 1 ))
        (else (frequency (cdr lst) elem))))
    )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; Problem 8 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define get-books
  ; answer here
  (lambda (lst)
      (cond
        ((null? lst) '())
        (else (append (cdr (car lst)) (get-books (cdr lst))))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; Problem 9 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


 (define make-record 
   ; Your answer here
   '()
   )
    
   