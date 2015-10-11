;; Exercise 4.1
;; Write a version of list-of-values that will evaluate operands from left-to-right.
;; Write a version of list-of-values that will evaluate operands from right-to-left.

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
	    (list-of-values (rest-operands exps) env))))

(define (list-of-values-l-r exps env)
  (if (no-operands? exps)
      '()
      (let (first (eval (first-operand exps) env))
	(cons first
	      (list-of-values-l-r (rest-operands exps) env)))))

(define (list-of-values-r-l exps env)
  (if (no-operands? exps)
      '()
      (let (rest (list-of-values-r-l (rest-operands exps) env))
	(cons (eval (first-operand exps) env)
	      rest))))
