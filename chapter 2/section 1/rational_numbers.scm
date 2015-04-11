(print-rat (add-rat (make-rat 1 -3) (make-rat 2 3)))
(print-rat (make-rat 1 -3))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (< d 0)
	(cons (/ (* n -1) g)
	      (/ (* d -1) g))
	(cons (/ n g)
	      (/ d g)))))

(define (gcd x y)
  (if (= y 0)
      x
      (gcd y (remainder x y))))

(define (numer x)
  (car x))

(define (denom x)
  (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
	       (* (numer y) (denom x)))
	    (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
	    (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
	    (* (numer y) (denom x))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))
