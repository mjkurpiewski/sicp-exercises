(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

;; Integral test for a circular region, abstract later maybe.
(define (integral-test radius origin boundary-1 boundary-2)
  (let ((center-x (car origin))
	(center-y (cadr origin))
	(trial-x (random-in-range (car boundary-1)
				  (cadr boundary-2)))
	(trial-y (random-in-range (car boundary-1)
				  (cadr boundary-2))))
    (< (+ (square (- trial-x center-x))
	  (square (- trial-y center-y)))
       (square radius))))

(define (estimate-integral predicate trials rect-1 rect-2)
  (let ((set-predicate (lambda () (predicate 1 (list 0 0) rect-1 rect-2))))
    (monte-carlo trials set-predicate)))

;; Monte Carlo experiment
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
	   (/ trials-passed trials))
	  ((experiment)
	   (iter (- trials-remaining 1) (+ trials-passed 1)))
	  (else
	   (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

;; Pseudo-random number generation
(define random-init 64902)

(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

(define (rand-update x)
  (let ((a (+ (expt 2 18) 1))
	(b 1)
	(m (expt 2 35)))
    (modulo (+ (* a x) b) m)))

;; Generating a random number in a given range
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))








