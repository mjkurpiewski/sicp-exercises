(define (fastexpt b n)
  (fastexpt-iter b n 1))

(define (fastexpt-iter b n a)
  (define (even? n)
    (= (remainder n 2) 0))

  (define (square n) (* n n))

  (cond ((= n 0) a)
	((even? n) (fastexpt-iter (square b) (/ n 2) a))
	(else (fastexpt-iter b (- n 1) (* a b)))))

;; square b, multiply times a, divide n by two so
;; call fastexpt-iter squared b (- n 2)  a 
