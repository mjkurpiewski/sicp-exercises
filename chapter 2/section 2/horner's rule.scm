(define (horner-eval x coefficient-sequence)
  (accumulate
   (lambda (this-coeff higher-terms)
     (+ this-coeff (* x higher-terms)))
   0
   coefficient-sequence))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op
		      initial
		      (cdr sequence)))))

;; The Horner's rule evaluation takes as input some value of x, 
;; as well as a list of the coefficients for the polynomial,
;; arranged from 0 to n. 

;; Horner's rule evaluates polynomials by starting from the nth term
;; , multiplying that value by the value of x, then summing that with
;; the (n-1)th term and multiplying by the value of x, continuing
;; on until the a0th term which is then added to the running sum.

;; So by this algorithm, we need not concern ourselves with eliminating
;; 0-coefficient terms, because it... or maybe we do?

;; Let's look at the evaluation of a polynomial by Horner's rule
;; Given polynomial: x^5 + 5x^3 + 3x +1
;; The sequence of coefficients for this polynomial, from 0 to n, is
;; (1 3 0 5 0 1)
;; This evaluates like...
;; ((1 * x) + 0) * x)
