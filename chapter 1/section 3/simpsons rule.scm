(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))

(define (increment n) (+ n 1))

(define (simpson-integral f a b n)
  (let ((h (/ (- b a) n)))
    (define (simpson-term k)
      (cond ((or (= k 0) (= k n)) (f (+ a (* k h))))
	    ((= (remainder k 2) 0) (* 2 (f (+ a (* k h)))))
	    (else (* 4 (f (+ a (* k h)))))))
    (* (/ h 3) (sum simpson-term 0 increment n))))
