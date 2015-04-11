(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

(define pair (cons 3 9))

;; this means that pair is (lambda (m) (m 3 9)). it's a function

(car pair)

;; we know car is defined as (z (lambda (p q) p)). car accepts an argument, z.
;; so (car pair) is (car (lambda (m) (m 3 9)))
;; so (car pair) is ((lambda (m) (m 3 9)) (lambda (p q) p))
;; so (car pair) is ((lambda (p q) p) 3 9)

(cdr pair)


 


