(define (fastmulti a b state)
  
  (define (even? n) 
    (= (remainder n 2) 0))

  (define (double n) 
    (+ n n))

  (define (halve n) 
    (if (even? n)
	(/ n 2)
	0))

  (cond ((= b 0) 0)
	((= b 1) a)
	((even? b) (double (fastmulti a (halve b))))
	(else (+ a (fastmulti a (- b 1))))))
