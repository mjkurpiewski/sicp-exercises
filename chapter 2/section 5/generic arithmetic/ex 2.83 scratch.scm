(define int1 (make-scheme-number 9))
int1

(define rat1 (raise int1))
rat1

(define real1 (raise rat1))
real1

(define rat2 (make-rational 3 4))
rat2

(define real2 (raise rat2))
real2

(real? real2)
(real? real1)

(define complex1 (raise real1))
(define complex2 (raise real2))
complex1
complex2
