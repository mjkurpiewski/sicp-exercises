#|
Define a procedure partial-sums that takes as argument a stream S
and returns the stream whose elements are:

S_0, S_0 + S_1, S_0 + S_1 + S_2, S_0 + S_1 + S_2 + S_3

For example: (partial-sums integers) should return
the stream: 1, 3, 6, 10, 15, ...

A stream of integers:
1, 2, 3, 4, 5, 6, 7, 8, 9

|#

(load "~/Programming/Scheme/sicp-exercises/chapter 3/section 5/streamprocedures.scm")

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1
			      (add-streams ones integers)))

(define (partial-sums stream)
  (cons-stream (stream-car stream)
	       (add-streams (partial-sums stream)
			    (stream-cdr stream))))
