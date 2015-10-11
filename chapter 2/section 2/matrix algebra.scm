(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (accumulate-n op initial sequences)
  (if (null? (car sequences))
      '()
      (cons (accumulate op 
			initial 
			(map (lambda (x) (car x)) sequences))
	    (accumulate-n op 
			  initial
			  (map (lambda (x) (cdr x)) sequences)))))

(define mat (list (list 1 2 3 4) 
		  (list 4 5 6 6)
		  (list 6 7 8 9)))

(define vect (list 4 5 6 7))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(define (transpose m)
  (accumulate-n cons '() m))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map <?> m)))
	
