(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else
           (error "Unknown op: 
            MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(define (make-from-mag-ang r d)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos d)))
	  ((eq? op 'imag-part) (* r (sin d)))
	  ((eq? op 'magnitude) r)
	  ((eq? op 'angle) d)
	  (else 
	   (error "Unknown op: MAKE-FROM-MAG-ANG" op))))
  dispatch)



