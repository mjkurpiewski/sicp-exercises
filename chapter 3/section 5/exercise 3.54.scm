#|
Define a procedure mul-streams, analogous to add-streams, that produces
the elementwise product of its two input streams. Use this together with
the stream of integers to complete the following definition of the stream
whose nth element (counting from 0) is n + 1 factorial.

(define factorials (cons-stream 1
				(mul-streams <?> <?>))
|#
(load "~/Programming/Scheme/sicp-exercises/chapter 3/section 5/streamprocedures.scm")
(load "~/Programming/Scheme/sicp-exercises/chapter 3/section 5/exercise 3.50.scm")

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))

(define factorials (cons-stream 1 (mul-streams factorials
					       (stream-map (lambda (x) (+ x 1)) integers))))


