;; Delay and force

(define (force delayed-object)
  (delayed-object))

(define (delay exp)
  (lambda () exp))

(define (memo-proc proc)
  (let ((already-run? false)
	(result false))
    (lambda ()
      (if (not already-run?)
	  (begin (set! result (proc))
		 (set! already-run? true)
		 result)
	  result))))

;; Stream construction and selection
(define (cons-stream a b)
  (cons a (delay b)))
(define (stream-car stream)
  (car stream))
(define (stream-cdr stream)
  (force (cdr stream)))















