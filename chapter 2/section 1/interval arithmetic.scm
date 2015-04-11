;; Extended Exercise: Interval Arithmetic

(define (add-interval x y)
  (make-interval (+ (lower-bound x) 
		    (lower-bound y))
		 (+ (upper-bound x)
		    (upper-bound y))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x)
		    (upper-bound y))
		 (- (upper-bound x)
		    (lower-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x)
	       (lower-bound y)))
	(p2 (* (lower-bound x)
	       (upper-bound y)))
	(p3 (* (upper-bound x)
	       (lower-bound y)))
	(p4 (* (upper-bound x)
	       (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
		   (max p1 p2 p3 p4))))

(define lambda-test (lambda (y) (and (<= (lower-bound y) 0.0) (>= (upper-bound y) 0.0))))

(define (div-interval x y)
  (if (and (<= (lower-bound y) 0.0) (>= (upper-bound y) 0.0))
   ;; ((lambda (a) (and (<= (lower-bound a) 0.0) (>= (upper-bound a) 0.0))) y)
      (error "Interval y spans zero.")
      (mul-interval x
		    (make-interval
		     (/ 1.0 (upper-bound y))
		     (/ 1.0 (lower-bound y))))))

(define (make-interval a b) (cons a b))
(define (make-center-width c w) (make-interval (- c w) (+ c w)))
(define (lower-bound i) (car i))
(define (upper-bound i) (cdr i))

;; Exercise 2.9

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

;; Exercise 2.12

(define (make-center-percent c p)
  (make-center-width c (* c (/ p 100.0))))

(define (percent i)
  (* 100.0 (/ (width i) (center i))))

;; Exercise 2.14

;; (par1 interval-1 interval-2) (2.310810810810811 . 3.6583710407239822)
(define (par1 r1 r2)
  (div-interval
   (mul-interval r1 r2)
   (add-interval r1 r2)))

;; (par2 interval-1 interval-2) (2.7081447963800906 . 3.121621621621622)
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval
     one
     (add-interval
      (div-interval one r1)
      (div-interval one r2)))))

;; Test

(define interval-1 (make-center-percent 5.0 5))
(define interval-2 (make-center-percent 7.0 10))
(define interval-3 (mul-interval interval-1 interval-2))
(define interval-sum (add-interval interval-1 interval-2))
(define interval-prod (mul-interval interval-1 interval-2))
(define interval-quot (div-interval interval-1 interval-2)) 
(define aquota (div-interval interval-1 interval-1))
(define aquotb (div-interval interval-1 interval-2))

;; a is 5%, center at 5, width is 0.25
;; (4.75, 5.25)
;; b is 10%, center at 7
;; 9.975 percent aquota, center 1.005
;; (0.947, 1.105) should be (1,1), center at 1, 0% tolerance
;; 14.925 percent aquotb, center 0.725

;; Exercise 2.15
;; Eva is correct. We know from 2.14 that Lem was also correct, in that par1 and par2 
;; produce meaningfully different results. The procedure div-interval only creates a
;; numerical approximation of the value of the operation. The center of aquota should be 1, but
;; it is 1.005. It's percent tolerance is also close to 10%, when the percent tolerance of aquota
;; should be 0%. 
;; When we transform the initial formula for parallel resistance, we multiplied by
;; (r1/r1) (r2/r2), which, algebraically, does evaluate to 1. However, the procedure div-internal
;; only provides the numerical approximation which introduces error in both r1 and r2, this error
;; is then amplified in calculation of par1. Therefore, par2 is the superior procedure.

;; Exercise 2.16
;; The procedure div-interval accomplishes it's task by multiplying it's first argument
;; by an interval constructed from the numerical representation of the reciprocal of it's
;; lower and upper bounds. So even when dividing one interval by itself, it must use approximations
;; of itself in the process of division because the second argument of div-interal is the approximate
;; numerical representation of that algebraic expression.  
