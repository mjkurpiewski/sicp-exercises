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

(define (make-rectangle p q)
;; construct a rectangle from a representation using two opposite corners
  (cons p q))

(define (rectangle-width r)
  (abs (- (x-point (car r)) (x-point (cdr r)))))

(define (rectangle-height r)
  (abs (- (y-point (car r)) (y-point (cdr r)))))


(define (make-rectangle-2 p h w)
  (cons p 
	(cons l w)))

(define (rectangle-width r)
  (cdr (cdr r)))

(define (rectangle-height r)
  (car (cdr r)))


(define (rectangle-perimeter r)
 (+ (* 2 (rectangle-width r)) (* 2 (rectangle-height r))))

(define (rectangle-area r)
  (* (rectangle-width r) (rectangle-height r)))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))
