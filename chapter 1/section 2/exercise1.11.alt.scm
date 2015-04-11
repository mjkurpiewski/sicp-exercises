(define (func-iter n)
  (define (iterator a b c count)
    (if (> count n)
	a
	(iterator (function a b c) a b (+ count 1))))
  (define (function a b c)
    (+ a (* 2 b) (* 3 c)))
  (if (< n 3)
      n
      (iterator 2 1 0 3)))
