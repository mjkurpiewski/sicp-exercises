(load "~/Programming/Scheme/sicp-exercises/chapter 3/section 5/streamprocedures.scm")

(define fibs
  (cons-stream 0
	       (cons-stream 1
			    (add-streams (stream-cdr fibs)
					 fibs))))
