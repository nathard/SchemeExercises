#lang eopl


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; Problem 1;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Solve Problem 1 here. Make sure that the function name is as mentioned in the 
 ; problem
(define insert-at
  (lambda (pos e lst)
   (cond
    ((> pos (length lst)) '())
     (else
      (if (zero? pos) (cons e lst) 
       (cons (car lst)
        (insert-at (- pos 1) e (cdr lst))))))))

    


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; Problem 2;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Solve Problem 2 here. Make sure that the function name is as mentioned in the 
 ; problem
(define prod
 (lambda (num lon)
  (if (null? lon) '()
   (cons 
    (if (> num (car lon)) (list num (car lon)) 
	 (list (car lon) num))
      (prod num (cdr lon))))))
	
(define max
 (lambda (lon1 lon2)
  (cond
   ((null? lon1) '())
        (else (append (prod (car lon1) lon2) (max (cdr lon1) lon2))))))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; Problem 3;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define naturals (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25))

; Solve Problem 3 here. Make sure that the function name is as mentioned in the 
 ; problem

(define prime
 (lambda (num lst)
  (cond
   ((null? lst) #t)
    ((eq? num 1) #f)
     ((or (eq? num (car lst)) (eq? (car lst) 1)) (prime num (cdr lst)))
      ((eq? 0 (modulo num (car lst))) #f)
       (else (prime num (cdr lst))))))    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; Problem 4;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Solve Problem 4 here. Make sure that the function name is as mentioned in the 
 ; problem

(define list-has-prime
    (lambda (lst)
      (if (null? lst) #f
        (or (prime (car lst) naturals) (list-has-prime (cdr lst))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; Problem 5;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Solve Problem 5 here. Make sure that the function name is as mentioned in the 
 ; problem

(define search
    (lambda (num btree)
	(let ((left (car (cdr btree)))
	 (right (car (cdr (cdr btree)))))
      (cond
        ((null? btree) #f)
        ((eq? num (car btree)) '())
        ((> num (car btree))
		 (if (search num right) (cons 'right (search num right)) 
		  (search num right)))
          (else (if (search num left) (cons 'left (search num left)) 
           (search num left)))))))

(define path
    (lambda (n btree)
      (let ((path (search n btree)))
        (if path path '()))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; Problem 6;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Solve Problem 6 here. Make sure that the function name is as mentioned in the 
 ; problem

(define row
    (lambda (num)
      (cond
        ((zero? num) '())
        ((odd? num) (cons 'b (row (- num 1) 'b 'w)))
        (else (cons 'w (row (- num 1) 'b 'w))))))

(define map
    (lambda (rows cols)
      (if (and (zero? rows) (zero? cols)) '()
         (cons (if (odd? rows)
          (row cols 'b 'w)
           (row cols 'w 'b)) (map (- rows 1) cols 'b 'w)))))

(define chessboard
    (lambda (val)
      (map val val)))    