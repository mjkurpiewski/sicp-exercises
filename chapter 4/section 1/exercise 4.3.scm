;; Exercise 4.3
;; Refactor eval so that it will perform dispatch in a data-directed style.
;; For instance, consider the differentiation procedure written during chapter 2.
;; For the sake of this exercise, we can consider the car of any given expression
;; to be the operation on which the expression should be dispatched.

(load "/home/mjkurpiewski/Programming/Scheme/sicp-exercises/chapter 4/section 1/optable.scm")
(load "/home/mjkurpiewski/Programming/Scheme/sicp-exercises/chapter 4/section 1/procedures.scm")

(define eval-table (make-eq-hash-table))
(hash-table/put! eval-table 'quote (lambda (exp env) (text-of-quotation exp)))
(hash-table/put! eval-table 'set! eval-assignment)
(hash-table/put! eval-table 'define eval-definition)
(hash-table/put! eval-table 'if eval-if)
(hash-table/put! eval-table 'cond (lambda (exp env) (eval (cond->if exp) env)))
(hash-table/put! eval-table 'lambda (lambda (exp env) (make-procedure (lambda-parameters exp)
							    (lambda-body exp)
							    env)))
(hash-table/put! eval-table 'begin (lambda (exp env) (eval-sequence (begin-actions exp)
							       env)))

(define (eval exp env)
  (let ((operation (hash-table/get eval-table (car exp) #f)))
    (cond ((self-evaluating? exp) exp)
	  ((variable? exp) (lookup-variable-value exp env))
	  ((operation) (operation exp env))
	  ((application? exp)
	   (apply (eval (operator exp) env)
		  (list-of-values (operands exp) env)))
	  (else
	   (error "Unknown expression type: EVAL" exp)))))

;; (define (eval exp env)
;;   (cond ((self-evaluating? exp) exp) ;; This will be the same. No car operator to dispatch.
;; 	((variable? exp) (lookup-variable-value exp env)) 
;; 	((quoted? exp) (text-of-quotation exp))
;; 	((assignment? exp) (eval-assignment exp env))
;; 	((definition? exp) (eval-definition exp env))
;; 	((if? exp) (eval-if exp env))
;; 	((lambda? exp) (make-procedure (lambda-parameters exp)
;; 				  (lambda-body exp)
;; 				  env))
;; 	((begin? exp) (eval-sequence (begin-actions exp) env))
;; 	((cond? exp) (eval (cond->if exp) env))
;; 	((application? exp)
;; 	 (apply (eval (operator exp) env)
;; 		(list-of-values (operands exp) env)))
;; 	(else
;; 	 (error "Unknown expression type: EVAL" exp))))
