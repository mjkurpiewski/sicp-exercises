;; Exercise 2.2

(define (make-segment p q)
  (cons p q))

(define (start-segment l)
  (car l))

(define (end-segment l)
  (cdr l))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (midpoint-segment l)
;;  (cons  (/ (+ (car (car l)) (car (cdr l))) 2)
;;	 (/ (+ (cdr (car l)) (cdr (cdr l))) 2)))
  (cons (/ (+ (x-point (start-segment l)) (x-point (end-segment l))) 2)
	(/ (+ (y-point (start-segment l)) (y-point (end-segment l))) 2)))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))


(midpoint-segment (make-segment (make-point -1 2) (make-point 3 -6)))
