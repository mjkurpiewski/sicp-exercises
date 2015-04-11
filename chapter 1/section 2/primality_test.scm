;; general algorithm is:
;; check if it's even? if so, go to the next index
;;            otherwise, evaluate timed-prime-test for a

(define (search-for-primes a b)
  (if (even? a) (search-for-primes (+ a 1) b)
      (cond ((< a b) (timed-prime-test a)
	     (search-for-primes (+ a 2) b)))))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) 
		       start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n)
	 n)
	((divides? test-divisor n)
	 test-divisor)
	(else (find-divisor
	       n
	       (+ test-divisor 1)))))

(define (even? a)
  (= (remainder a 2) 0))

(define (divides? a b)
  (= (remainder b a) 0))


(search-for-primes 1000 1020)
;; smallest divisor of 199 is 199
;; smallest divisor of 1999 is 1999
;; smallest divisor of 19999 is 7
