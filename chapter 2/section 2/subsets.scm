(define (subsets S)
  (if (null? S)
      (list '())
      (let ((rest (subsets (cdr S))))
	(append rest
		(map (lambda (x)
		       (append (list (car S))
			       x)) 
		     rest)))))
		

(define testset (list 1 2 3))
