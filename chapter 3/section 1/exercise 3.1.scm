; (define A (make-accumulator 5)
; (A 10)
; 15
; (A 10)
; 25

(define (make-accumulator n)
  (let ((value n))
    (lambda (input)
      (begin (set! value (+ value input))
	     value))))
