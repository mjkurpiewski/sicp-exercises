(define x (list 'a 'b))
(define z1 (cons x x))
(define z2 (cons (list 'a 'b)
		 (list 'a 'b)))

(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)

(define (make-cycle x)
  (set-cdr! (cdr x) x)
  x)


;; Bus trip
(+ (* 3 (* 4 102.20)) (* 3 40) (* 3 116.50)) ; $1695
(+ (* 89.0 3) (* 3 (* 4 34)) (* 3 110)) ; $1000
