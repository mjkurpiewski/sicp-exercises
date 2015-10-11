(load "~/Programming/Scheme/sicp-exercises/chapter 3/section 5/streamprocedures.scm")
#|
This is a problem in which we are tasked
with enumerating the positive integers
with no prime factors other than 2, 3, or 5.

The exercise frames the problem in the following
fashion:

We have a stream S that begins with 1.
Elements of (scale-stream S 2) are elements of S
Elements of (scale-stream S 3) are elements of S
Elements of (scale-stream S 5) are elements of S
These are the elements of S.

We are then provided a function, merge, which will
merge two streams, order them, and remove duplicates.
Code follows.
|#

(define (scale-stream stream factor)
  (stream-map (lambda (x)
		(* x factor)) stream))

(define (merge s1 s2)
  (cond ((null? s1) s2)
	((null? s2) s1)
	(else
	 (let ((s1car (stream-car s1))
	       (s2car (stream-car s2)))
	   (cond ((< s1car s2car)
		  (cons-stream s1car
			       (merge (stream-cdr s1)
				      s2)))
		 ((> s1car s2car)
		  (cons-stream
		   s2car
		   (merge s1
			  (stream-cdr s2))))
		 (else
		  (cons-stream
		   s1car
		   (merge (stream-cdr s1)
			  (stream-cdr s2)))))))))

;; We then, in terms of merge, will define our stream
;; as
;; (define S (cons-stream 1 (merge <?> <?>)))

(define S (cons-stream 1 (merge (scale-stream S 2)
				(merge (scale-stream S 3)
				       (scale-stream S 5)))))
