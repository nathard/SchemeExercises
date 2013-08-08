#lang racket

(require (lib "eopl.ss" "eopl"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;; Problem 1;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Procedures for procedural representation of the environment
; Solve using these procedures.

 (define report-no-binding-found
    (lambda (search-var)
      (eopl:error "No binding for ~s" search-var)))


  ;; empty-env : () -> Env
  (define empty-env
    (lambda ()
      (lambda (search-var)  
        (report-no-binding-found search-var))))
  
  ;; extend-env : Var * Schemeval * Env -> Env
  (define extend-env
    (lambda (saved-var saved-val saved-env)
      (lambda (search-var)
            (if (eqv? search-var saved-var)
                saved-val
                (apply-env saved-env search-var)))))
  
  ;; apply-env : Env * Var -> Schemeval
  (define apply-env
    (lambda (env search-var) 
      (env search-var)))
  
  (define empty-env?
    (lambda (search-var)
	(eq? search-var (empty-env))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;; Problem 2;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 ; Solve problem 2 here.
 ; 2.a Write answer as comments
 
  ;; empty-stack : () -> stack
  ;; empty-stack? : stack -> bool
  ;; push : val * stack -> stack
  ;; top : stack -> val | bool
  ;; pop : stack -> stack | bool
 
 
 ;2.b Write your procedures here

 (define apply-func
  (lambda (func s)
    (func s)))
  
 (define empty-stack
   (lambda () 
     (lambda () (void))))
 
 (define push
  (lambda (e s)
    (lambda (n)
      (if (eq? n 0) e s))))
   
 (define pop
  (lambda (s)
    (if (empty-stack? s) #f 
      (apply-func 0 s))))
   
 (define top
  (lambda (s)
    (if (empty-stack? s) #f 
      (apply-func 1 s))))
   
 (define empty-stack?
   (lambda (s)
     (eq? s (empty-stack))))
  
  
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;; Problem 3;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a.
  ; Write solutions in terms of comments.
  
 ;; empty-dict : () -> dict
 ;; add : word * meaning * dict -> dict
 ;; look : word * dict -> lom | ()
 ;; empty-dict? : dict -> bool

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;b.
  ; Write solutions in terms of comments.
  
  ; Constructors: empty-dict, add
  ; Observers: empty-dict?, look

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; c
  ; Write your procedures here
  
(define empty-dict
  (lambda () '()))

(define empty-dict?
  (lambda (dict)
    (null? dict)))

(define add
  (lambda (word meaning dict)
    (list word meaning dict)))

(define look
  (lambda (word dict)
    (cond
      ((null? dict) '())
        ((eq? word (car dict)) (cadr dict))
          (else (look word (cddr dict))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;d
  ; Write your procedures here

(define empty-dict-p?
  (lambda (dict)
	(eq? dict (empty-dict-p))))
	
	
(define empty-dict-p
    (lambda ()
      (lambda (search-var)  
        '())))

(define add-p
    (lambda (word meaning dict)
      (lambda (search-var)
            (if (eqv? search-var word)
                meaning
                (look-p search-var dict)))))

(define look-p
    (lambda (word dict) 
      (dict word)))
  
  
  

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;; Problem 4;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a.
  ; Write solutions in terms of comments.
  
 ;; empty-tree : () -> tree
 ;; empty-tree? : tree -> bool
 ;; is-red? : tree -> bool
 ;; is-blue? : tree -> bool
 ;; add-red :  val * tree * tree -> tree
 ;; add-blue : val * tree -> tree
 ;; sum-red : tree -> val

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;b.
  ; Write solutions in terms of comments.
  
  ; Constructors: empty-tree, add-red, add-blue
  ; Observers: empty-tree?, is-red?, is-blue?, sum-red

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; c
  ; Write your procedures here
  
(define empty-tree
    (lambda () '()))

(define empty-tree?
  (lambda (tree)
    (null? tree)))

(define is-red? 
  (lambda (tree)
    (if
      (null? tree) #t
       ((and (number? (car tree)) 
        (not (null? (left tree))) 
          (not (null? (right tree))))))))

(define is-blue? 
  (lambda (tree)
    (if (null? tree) #t
        ((and (number? (car tree)) 
          (not (null? (left tree))) 
            (null? (right tree)))))))

(define add-red
  (lambda (n tree1 tree2)
    (list (n tree1 tree2))))

(define add-blue
  (lambda (n tree)
    (list n tree)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;d
  ; Write your procedures here

  