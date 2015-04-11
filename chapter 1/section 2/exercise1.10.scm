(define (A x y)
  (cond ((= y 0) 0)
	((= x 0) (* 2 y))
	((= y 1) 2)
	(else (A (- x 1) (A x (- y 1))))))
;; (A 1 10) = 1024
;; 1 = 2? so 2^10
;; (A 2 4) = 65536
;; (A 3 3) = 65536

;; (define (f n) (A 0 n))
;; 0,5 = 10
;; 0,20 = 20
;; Returns 2 times n

;; (define (g n) (A 1 n))
;; returns 2 to the nth power

;; (define (h n) (A 2 n))
;; 2,1 = 2
;; 2,2 = 4
;; 2,3 = 16:
;; 2,4 = 65536
;; 2^2^2... to n 
