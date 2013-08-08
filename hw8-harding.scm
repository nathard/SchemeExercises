#lang eopl

; This file contains starting code for Homework 8 in Com S 342 
; Much of this is derived from the source code for EOPL, 3rd edition.

  (require (lib "chapter3/let-lang/drscheme-init.scm" "eopl3")) ; for run-tests!

   ; PROBLEM 1
    ; This is a sample of problem 1 a
   (define sample-run
      (lambda()
        (run "-(1, 2)")))
        
   ; This is a sample of problem 1 b
    (define sample-scan
      (lambda()
        (scan&parse "-(1, 2)")))
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; Now UNCOMMENT the lines below and write your answers for problem 1
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
;    ; Your answers:    
    (define run-const-exp
      (lambda()
;    ; write an expression that runs a const-exp here 
      (run "11")  
        ))
;    
    (define run-diff-exp
      (lambda()
;    ; write an expression that  runs a diff-exp here 
       (run "-(8, 3)") 
        ))
;    
    (define run-zero?-exp
      (lambda()
;    ; write an expression that  runs a zero?-exp here 
        (run "zero? (0)")
        ))
;    
    (define run-if-exp
      (lambda()
;    ; write an expression that  runs a if-exp here 
       (run "if zero? (0) then x else y") 
        ))
;
    (define run-var-exp
      (lambda()
;    ; write an expression that  runs a var-exp here 
       (run "x") 
        ))
;
    (define run-let-exp
      (lambda()
;    ; write an expression that  runs a var-exp here 
       (run "let p = 5 in -(p, 3)") 
        ))
  
    (define scan-const-exp
      (lambda()
;    ; write an expression that scans a const-exp here 
       (scan&parse "11") 
        ))
;    
    (define scan-diff-exp
      (lambda()
;    ; write an expression that scans a diff-exp here 
      (scan&parse "-(1, 2)")  
        ))
;    
    (define scan-zero?-exp
      (lambda()
;    ; write an expression that scans a zero?-exp here 
        (scan&parse "zero? (0)")
        ))
;    
    (define scan-if-exp
      (lambda()
;    ; write an expression that scans a if-exp here 
      (scan&parse "if zero? (0) then x else y")  
        ))
;
    (define scan-var-exp
      (lambda()
;    ; write an expression that scans a var-exp here 
      (scan&parse "x")  
        ))
;
    (define scans-let-exp
      (lambda()
;    ; write an expression that scans a var-exp here 
      (scan&parse "let p = 5 in -(p, 3)")  
        ))
 
    
  
  
  
  ;;;;;;;;;;;;;;;; TOP LEVEL FUNCTION ;;;;;;;;;;;;;;;;
  
  ;; run : String -> ExpVal
  ;; Page: 71
  (define run
    (lambda (string)
      (value-of-program (scan&parse string))))

  ;;;;;;;;;;;;;;;; GRAMMATICAL SPECIFICATION ;;;;;;;;;;;;;;;;

  ;; Following defines legal tokens in the let language
  (define the-lexical-spec
    '((whitespace (whitespace) skip)
      (comment ("%" (arbno (not #\newline))) skip)
      (identifier
       (letter (arbno (or letter digit "_" "-" "?")))
       symbol)
      (number (digit (arbno digit)) number)
      (number ("-" digit (arbno digit)) number)
      ))

  ; Following defines the grammar for the let language
  ; In the description symbols enclosed in "term" represent
  ; terminals and others are non-terminals. For example, 
  ; "(" is a terminal and so is "zero?" as these do not 
  ; require further derivation. Note that the terminals
  ; number and identifier are defined by the lexical 
  ; specification above.
  (define the-grammar
    ; program ::= expression
    '((program (expression) a-program)
      
      ; expression := number
      (expression (number) const-exp)
      
      ; expression := "-" "(" expression "," expression ")"
      (expression
        ("-" "(" expression "," expression ")")
        diff-exp)   
      
      ; expression := "zero?" "(" expression ")"      
      (expression
       ("zero?" "(" expression ")")
       zero?-exp)
      
      ; expression := "if" expression "then" expression "else" expression 
      (expression
       ("if" expression "then" expression "else" expression)
       if-exp)
      
      ; expression := identifier
      (expression (identifier) var-exp)
      
      ; expression := "let" identifier "=" expression "in" expression
      (expression
       ("let" identifier "=" expression "in" expression)
       let-exp)
      
      ; expression := "/" "(" expression "," expression ")"
      (expression 
       ("/" "(" expression "," expression ")") 
       div-exp)
      
       ; expression := "+" "(" separated-list expression ","")"
      (expression
       ("+" "(" (separated-list expression ",") ")" )
       add-exp)
      
      ; expression := "*" "(" separated-list expression ","")"
      (expression
        ("*" "(" (separated-list expression ",") ")" )
        mult-exp)
      
      ; expression := "list" "(" expression ("," expression)* ")"
      (expression
        ("list" "(" (separated-list expression ",") ")" )
        list-exp)
      
      ; expression ::= "unpack" {identifier}* = expression in expression
      (expression
       ("unpack" (arbno identifier) "=" expression "in" expression)
       unpack-exp)
      
      ; expression := "empty-stack"
      (expression 
       ("empty-stack")
       empty-stack-exp)
      
      ; expression := "push" "(" expression expression ")"
      (expression
       ("push" "(" expression expression ")")
       push-exp)
      
      ; expression := "pop" "(" expression ")"
      (expression
       ("pop" "(" expression ")")
       pop-exp)
      
      ; expression := "top" "(" expression ")"
      (expression
       ("top" "(" expression ")")
       top-exp)
      
      ; expression := "empty-stack?" "(" expression ")"
      (expression
       ("empty-stack?" "(" expression ")")
       empty-stack?-exp)
      
      ))

  ;;;;;;;;;;;;;;;; SLLGEN BOILERPLATE ;;;;;;;;;;;;;;;;

  (sllgen:make-define-datatypes the-lexical-spec the-grammar)

  (define show-the-datatypes
    (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

  (define scan&parse
    (sllgen:make-string-parser the-lexical-spec the-grammar))

  (define just-scan
    (sllgen:make-string-scanner the-lexical-spec the-grammar))

  ;;;;;;;;;;;;;;;; EXPRESSED VALUES ;;;;;;;;;;;;;;;;
  ;;; an expressed value is either a number or a boolean.

  (define-datatype expval expval?
    (num-val
      (value number?))
    (bool-val
      (boolean boolean?))
    ; Problem 3: this data-type is already extended
    (list-val
      (list list?))
    ; Problem 5:
    (stack-val
      (stk stack?))
    (push
      (value expval?) (stk stack?))
    (empty-stack))
    
  ;;; predicates
  (define stack?
    (lambda (exp)
      (cases expval exp
        (stack-val (s) #t)
        (push (s e) #t)
        (empty-stack () #t)
          (else #f))))
  
  
  ;;; extractors:

  ;; expval->num : ExpVal -> Int
  ;; Page: 70
  (define expval->num
    (lambda (v)
      (cases expval v
	(num-val (num) num)
	(else (expval-extractor-error 'num v)))))

  ;; expval->bool : ExpVal -> Bool
  ;; Page: 70
  (define expval->bool
    (lambda (v)
      (cases expval v
	(bool-val (bool) bool)
	(else (expval-extractor-error 'bool v)))))
  
  ; Problem 3
  (define expval->list
    (lambda (v)
      (cases expval v
        (list-val (lst) lst)
        (else (expval-extractor-error 'list v)))))
  
  ;Problem 5
  (define expval->stack
    (lambda (v)
      (cases expval v
        (push (s e) s)
        (stack-val (s) s)
        (else (expval-extractor-error 'stack v)))))
  
  (define expval->stack-val
    (lambda (v)
      (cases expval v
        (push (s e) s)
        (else (expval-extractor-error 'stack v)))))

  (define expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
	variant value)))

  ;;;;;;;;;;;;;;;; ENVIRONMENT STRUCTURE ;;;;;;;;;;;;;;;;
  ;; example of a data type built without define-datatype
  (define empty-env-record
    (lambda () 
      '()))

  (define extended-env-record
    (lambda (sym val old-env)
      (cons (list sym val) old-env)))

  (define empty-env-record? null?)

  (define environment?
    (lambda (x)
      (or (empty-env-record? x)
          (and (pair? x)
               (symbol? (car (car x)))
               (expval? (cadr (car x)))
               (environment? (cdr x))))))

  (define extended-env-record->sym
    (lambda (r)
      (car (car r))))

  (define extended-env-record->val
    (lambda (r)
      (cadr (car r))))

  (define extended-env-record->old-env
    (lambda (r)
      (cdr r)))

  ;;;;;;;;;;;;;;;; INITIAL ENVIRONMENT ;;;;;;;;;;;;;;;;
  ;; init-env : () -> Env
  ;; usage: (init-env) = [i=1, v=5, x=10]
  ;; (init-env) builds an environment in which i is bound to the
  ;; expressed value 1, v is bound to the expressed value 5, and x is
  ;; bound to the expressed value 10.
  ;; Page: 69

  (define init-env 
    (lambda ()
      (extend-env 
       'i (num-val 1)
       (extend-env
        'v (num-val 5)
        (extend-env
         'x (num-val 10)
         (empty-env))))))
  
  ;;;;;;;;;;;;;;;; ENVIRONMENT CONSTRUCTORS AND OBSERVERS ;;;;;;;;;;;;;;;;
  (define empty-env
    (lambda ()
      (empty-env-record)))


  (define empty-env? 
    (lambda (x)
      (empty-env-record? x)))

  (define extend-env
    (lambda (sym val old-env)
      (extended-env-record sym val old-env)))

  (define apply-env
    (lambda (env search-sym)
      (if (empty-env? env)
	(eopl:error 'apply-env "No binding for ~s" search-sym)
	(let ((sym (extended-env-record->sym env))
	      (val (extended-env-record->val env))
	      (old-env (extended-env-record->old-env env)))
	  (if (eqv? search-sym sym)
	    val
	    (apply-env old-env search-sym))))))
  
  ; Problem 4
  
  (define extend-env*
    (lambda (lst1 lst2 env)
      (if (null? lst1) env
          (if (null? (cdr lst1))
             (extend-env (car lst1) (car lst2) env)
               (extend-env* (cdr lst1) (cdr lst2) (extend-env (car lst1) (car lst2) env))))))
  
  ;;;;;;;;;;;;;;;; THE INTERPRETER ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  ;; Page: 71
  (define value-of-program 
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 71
  (define value-of
    (lambda (exp env)
      (cases expression exp

        ;Semantics of constant expression
        (const-exp (num) (num-val num))

        ;Semantics of variable expression
        (var-exp (var) (apply-env env var))

        ;Semantics of difference expression
        (diff-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (- num1 num2)))))

        ;Semantics of zero test expression
        (zero?-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (let ((num1 (expval->num val1)))
              (if (zero? num1)
                (bool-val #t)
                (bool-val #f)))))
              
        ;Semantics of if expression
        (if-exp (exp1 exp2 exp3)
          (let ((val1 (value-of exp1 env)))
            (if (expval->bool val1)
              (value-of exp2 env)
              (value-of exp3 env))))

        ;Semantics of let expression
        (let-exp (var exp1 body)       
          (let ((val1 (value-of exp1 env)))
            (value-of body
              (extend-env var val1 env))))
        
        ;Semantics of div expression      
        (div-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
                (/ num1 num2)))))
        
        ;Semantics of add expression 
        (add-exp (exp1) 
          (num-val (if (null? exp1) 0
            (+ (expval->num (value-of (car exp1) env)) 
              (expval->num (value-of (add-exp (cdr exp1)) env))))))
        
        ;Semantics of mult expression 
        (mult-exp (exp1) 
          (num-val (if (null? exp1) 1
            (* (expval->num (value-of (car exp1) env)) 
	      (expval->num (value-of (mult-exp (cdr exp1)) env))))))
        
        ;Semantics of list expression
        (list-exp (exp1) 
          (list-val (if (null? exp1) '()
            (cons (value-of (car exp1) env) 
	      (expval->list (value-of (list-exp (cdr exp1)) env))))))
        
        ;Semantics of unpack expression
        (unpack-exp (lst exp1 exp2) 
          (if (eq? (length (expval->list (value-of exp1 env))) (length lst)) 
            (value-of exp2 (extend-env* lst (expval->list (value-of exp1 env)) env))
              (eopl:error 'error "number of identifiers and values are unequal in list")))
        
        ;Semantics of stack
        
        (empty-stack-exp () 
          (stack-val (empty-stack)))
        
        (push-exp (exp stk) 
          (stack-val (push (value-of exp env) (expval->stack (value-of stk env)))))

        (pop-exp (stk) 
          (stack-val (expval->stack (expval->stack (value-of stk env)))))
        
        (top-exp (stk) 
          (expval->stack-val (expval->stack (value-of stk env))))
        
        (empty-stack?-exp (stk) 
          (bool-val (equal? (empty-stack) (expval->stack (value-of stk env)))))
        
        )))

  
  ;;;;;;;;;;;;;;;; OTHER FUNCTIONS ;;;;;;;;;;;;;;;;
  ;;;;;; HELPFUL FOR TESTING AND DEBUGGING ;;;;;;;;
 
  ;; run-all : () -> unspecified
  ;; runs all the tests in test-list, comparing the results with
  ;; equal-answer?  
  
  (define test-list
    '(
       (plus1 "+(1, 1)" 2)
       (plus2 "+(x, 3)" 13)
       (plus3 "+(1, 2, 3)" 6)

       (mult1 "*(1, 2)" 1)
       (mult2 "*(2, 2, 2)" 8)
       (mult3 "*(2, x)" 20)
         (div1 "/(1, 1)" 1)
         (div2 "/(2, 1)" 2)
         (div3 "/(20, x)" 2)
         (list1 "list(1, 2, 3)" (1 2 3))
         (list2 "list()" ())
         (list3 "list(x)" (10))
         (unpack1 "unpack x y z = list(1 2 3) in - (z, 3)" 0)
         (unpack1 "unpack x = list(0) in zero?(x)" #t) 
         ))
     

  (define run-all
    (lambda ()
      (run-tests! run equal-answer? test-list)))
  
  (define equal-answer?
    (lambda (ans correct-ans)
      (equal? ans (sloppy->expval correct-ans))))
  
 (define sloppy->expval 
    (lambda (sloppy-val)
      (cond
        ((number? sloppy-val) (num-val sloppy-val))
        ((boolean? sloppy-val) (bool-val sloppy-val))
        ((list? sloppy-val) (list-val 
                             (sloppylst->expvallst sloppy-val)))
        (else
         (eopl:error 'sloppy->expval 
                     "Can't convert sloppy value to expval: ~s"
                     sloppy-val)))))


  
  (define sloppylst->expvallst
    (lambda (sloppy-lst)
      (cond
        ((null? sloppy-lst) ())
        (else (cons (sloppy->expval (car sloppy-lst))
                    (sloppylst->expvallst (cdr sloppy-lst)))))))
    
  ;; run-one : symbol -> expval
  ;; (run-one sym) runs the test whose name is sym  
  (define run-one
    (lambda (test-name)
      (let ((the-test (assoc test-name test-list)))
        (cond
          ((assoc test-name test-list)
           => (lambda (test)
                (run (cadr test))))
          (else (eopl:error 'run-one "no such test: ~s" test-name))))))
 
  ;; run-these-tests : () -> unspecified
  ;; runs these test given as argument, comparing the results with
  ;; equal-answer?  
  (define run-these-tests
    (lambda (tests)
      (run-tests! run equal-answer? tests)))

  
  ;; (with-trace) run fun while tracing important calls in interpreter.
  (define with-trace
    (lambda (fun)
      (begin 
        (trace run)
        (trace scan&parse)
        (trace value-of-program)
        (trace value-of)
        (fun)
        (untrace run)
        (untrace scan&parse)
        (untrace value-of-program)
        (untrace value-of)
        )))
  
  ;; (run-with-trace) traces important calls in interpreter.
  (define run-with-trace
    (lambda (string)
      (with-trace
       (lambda ()
         (run string)))))

    ;; (run-one-with-trace) runs a test while tracing important calls in interpreter.
  (define run-one-with-trace
    (lambda (test-name)
      (with-trace
        (lambda ()
        (run-one test-name)))))

  ;; (runall-with-trace) similar to run-one-with-trace, but runs all tests.
  (define runall-with-trace
    (lambda ()
      (with-trace run-all)))
  
  ; TESTS for HW 7 
  (define test-plus 
    (lambda ()
      (run-these-tests
       '(
         (plus1 "+(1, 1)" 2)
         (plus2 "+(x, 3)" 13)
         (plus3 "+(1, 2, 3)" 6)
          ))))

  (define test-mult
    (lambda ()
      (run-these-tests
       '(
         (mult1 "*(1, 2)" 2)
         (mult2 "*(2, 2, 2)" 8)
         (mult3 "*(2, x)" 20)
         ))))

  (define test-div
    (lambda ()
      (run-these-tests
       '(
         (div1 "/(1, 1)" 1)
         (div2 "/(2, 1)" 2)
         (div3 "/(20, x)" 2)
         ))))
  
  
  (define test-lists
    (lambda()
      (run-these-tests
       '(
         (list1 "list(1, 2, 3)" (1 2 3))
         (list2 "list()" ())
         (list3 "list(x)" (10))
         ))))
  
  (define test-unpack
    (lambda()
      (run-these-tests
       '(
         (unpack1 "unpack x y z = list(1, 2, 3) in -(z, 3)" 0)
         (unpack1 "unpack x = list(0) in zero?(x)" #t) 
         ))))
