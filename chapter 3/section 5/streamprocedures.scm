;; Generalized stream procedures
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc . argstreams)
  (if (null? (car argstreams))
      '()
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
	      (cons proc
		    (map stream-cdr
			 argstreams))))))

(define (scale-stream stream factor)
  (stream-map (lambda (x)
		(* x factor)) stream))

(define (stream-for-each proc s)
  (if (null? s)
      'done
      (begin
	(proc (stream-car s))
	(stream-for-each proc
			 (stream-cdr s)))))

(define (stream-filter pred stream)
  (cond ((null? stream)
	 '())
	((pred (stream-car stream))
	 (cons-stream
	  (stream-car stream)
	  (stream-filter
	   pred
	   (stream-cdr stream))))
	(else (stream-filter
	       pred
	       (stream-cdr stream)))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      '()
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1)
				  high))))

;; Stream arithmetic
(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

;; Display procedures
(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

;; Some useful streams
(define ones (cons-stream 1 ones))
(define integers (cons-stream 1
			      (add-streams ones integers)))
