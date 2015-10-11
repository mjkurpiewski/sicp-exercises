(define (make-rand rng-seed)
  (let ((current-rand 1))

    (define (debug)
      (display "Current seed: ")
      (display rng-seed)
      (newline)
      (display "Current rand: ")
      (display current-rand)
      (newline))
    
    (define (reset new-seed)
      (begin (set! rng-seed new-seed)
	     #t))

    (define (generate)
      (let ((a 48271)
	    (c 0)
	    (m (- (expt 2 31) 1)))
	(set! current-rand (modulo (+ (* a current-rand) c) m))
	current-rand))

    (define (dispatch . args)
      (cond ((eq? (car args) 'generate) (generate))
	    ((eq? (car args) 'reset) (reset (cadr args)))
	    ((eq? (car args) 'debug) (debug))
	    (else (error "Invalid arguments."))))
    dispatch))

