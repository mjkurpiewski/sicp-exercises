;; Exercise 2.6

;; 

;; zero is an anonymous function with parameter f that returns another anonymous function
;; which takes parameter x and returns itself (identity)
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;; evaluation of (add-1 zero)

(lambda (f) (lambda (x) (f ((zero f) x))))
(lambda (f) (lambda (x) (f ((lambda (x) x) x))))
(lambda (f) (lambda (x) (f x)))

;; evaluation of (add-1 one)

(lambda (f) (lambda (x) (f ((one f) x))))
(lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))
(lambda (f) (lambda (x) (f (f x))))

;; direct definition of the addition procedure

(define (add a b)
  (lambda (f) (lambda (x) ((a f) (b f) x))))
