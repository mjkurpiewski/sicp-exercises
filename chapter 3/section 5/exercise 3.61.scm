(load "~/Programming/Scheme/sicp-exercises/chapter 3/section 5/streamprocedures.scm")

#|
Let S be a power series whose constant term is 1.
Suppose we want to find the power series 1/S, that is
the series X such that (* S X) = 1

If we write S = 1 + S_r, where S_r is the rest that
follows the constant, we can solve for X:

(* S X) = 1
(* X (+ S_r 1)) = 1
(+ X (* S_r X)) = 1
X = (- 1 (* S_r X))

Therefore, X represents the power series whose 
constant term is 1 and whose higher-order terms
are given by the negative of S_r multiplied by X.
|#

(define (integrate-series power-series)
  (mul-streams (stream-map (lambda (x) (/ 1 x)) integers)
	       power-series))

(define (negate stream)
  (stream-map (lambda (x) (- x)) stream))

(define exp-series
  (cons-stream 1
	       (integrate-series exp-series)))

(define cosine-series
  (cons-stream 1
	       (negate (integrate-series sine-series))))

(define sine-series
  (cons-stream 0
	       (integrate-series cosine-series)))

;; Determines the product of two series.

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
	       (add-streams (add-streams (scale-stream (stream-cdr s1)
						       (stream-car s2))
					 (scale-stream (stream-cdr s2)
						       (stream-car s1)))
			    (cons-stream 0 (mul-series (stream-cdr s1)
						       (stream-cdr s2))))))


