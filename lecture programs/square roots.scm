(define (sqrt x)
  (fixed-point
   (average-damp (lambda (y) (/ x y)))
   1))

(define average-damp
  (lambda (f) (lambda (x) (average (f x) x))))
		 
