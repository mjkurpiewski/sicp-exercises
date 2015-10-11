(load "~/Programming/Scheme/sicp-exercises/chapter 3/section 5/streamprocedures.scm")

#|
Define a procedure (integrate-series stream), where stream is representative of the
coefficients of a power series, which returns a stream where the terms are multiplied
by decreasing fractions (1/1, 1/2, 1/3, 1/4, 1/5...)

An approach, perhaps: multiply the given stream by a stream of integers to which a map
of the function (lambda (x) (/ 1 x)) is applied. 
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


