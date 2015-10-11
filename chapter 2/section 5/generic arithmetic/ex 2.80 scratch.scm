

Exercise 2.80: Define a generic predicate =zero? that tests if its argument is zero, and install it in the generic arithmetic package. This operation should work for ordinary numbers, rational numbers, and complex numbers. 

For a 'scheme-number: (= x 0)
For a 'rational: (= (numer x) 0)
For a 'complex: (and (= (real-part x) 0)
		     (= (imag-part x) 0))

(define schemezero (make-scheme-number 0))
(define rationalzero (make-rational 0 5))
(define complexzero (make-complex-from-real-imag 0 0))

(=zero? schemezero)
(=zero? rationalzero)
(=zero? complexzero)
(=zero? (make-rational 3 4))
