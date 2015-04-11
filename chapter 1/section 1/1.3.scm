(define (square x)
  (* x x))

(define (sum-squares x y)
  (+ (square x) (square y)))

(define (sum-largest-squares a b c)
  (cond ((and (>= a b) (> a c) (>= b c)) (sum-squares a b))
	((and (> a b) (> c b)) (sum-squares a c))
	(else (sum-squares b c))))
