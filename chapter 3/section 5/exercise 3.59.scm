(load "~/Programming/Scheme/sicp-exercises/chapter 3/section 5/streamprocedures.scm")

#|
Define a procedure (integrate-series stream), where stream is representative of the
coefficients of a power series, which returns a stream where the terms are multiplied
by decreasing fractions (1/1, 1/2, 1/3, 1/4, 1/5...)

An approach, perhaps: multiply the given stream by a stream of integers to which a map
of the function (lambda (x) (/ 1 x)) is applied. 
|#

(define (integrate-series power-series)
  (mul-streams power-series
	       (stream-map (lambda (x) (/ 1 x)) integers)))

(define exp-series
  (cons-stream 1
	       (integrate-series exp-series)))

(define cosine-series
  (cons-stream 1
	       (integrate-series sine-series)))

(define sine-series
  (cons-stream 0
	       (integrate-series cosine-series)))

;; Little snippits from testing

(map (lambda (x) (stream-ref cosine-series x)) (list 0 1 2 3 4 5))

(map (lambda (x) (stream-ref sine-series x)) (list 0 1 2 3 4 5))

