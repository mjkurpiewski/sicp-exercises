(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
	(mutex 'acquire)
	(let ((val (apply p args)))
	  (mutex 'release)
	  val))
      serialized-p)))

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
	     (if (test-and-set! cell)
		 (the-mutex 'acquire)))
	    ((eq? m 'release)
	     (begin (clear! cell)
		    (display "Mutex released")))))
    the-mutex))

(define (clear! cell)
  (set-car! cell false))

(define (test-and-set! cell)
  (without-interrupts
   (lambda ()
     (if (car cell)
	 true
	 (begin (set-car! cell true)
		(display "Mutex acquired")
		false)))))

(define (mutex-semaphore)
  (let ((mutexes (list (make-mutex))))
    (define (acquire i)
      (if (<= i (- (length mutexes) 1))
	  ((list-ref mutexes i) 'acquire)
	  (error "Index out of range")))
    (define (release i)
      (if (<= i (- (length mutexes) 1))
	  ((list-ref mutexes i) 'release)
	  (error "Index out of range.")))
    (define (new-mutex)
      (begin (append! mutexes (list (make-mutex)))
	     (display "New mutex index is: ")
	     (display (- (length mutexes) 1))))
    (define (semaphore-activity m)
      (cond ((eq? m 'acquire) acquire)
	    ((eq? m 'release) release)
	    ((eq? m 'new-mutex) (new-mutex))))
    semaphore-activity))

(define (test-and-set-semaphore)
  (let ((signals (make-vector 1 false)))
    (define (vector-clear! i)
      (vector-set! signals i false))
    (define (vector-test-and-set! i)
      (without-interrupts
       (lambda ()
	 (if (vector-ref signals i)
	     true
	     (begin (vector-set! signals i true)
		    (display "Mutex acquired.")
		    'ok)))))
    (define (acquire i)
      (if (> i (vector-length signals))
	  (error "Index out of bounds.")
	  (vector-test-and-set! i)))
    (define (release i)
      (begin (vector-clear! i)
	     'ok))
    (define (inspect i)
      (begin (display "Mutex at INDEX ")
	     (display i)
	     (display " has value: ")
	     (display (vector-ref signals i))))
    (define (new-signal)
      (begin (set! signals (vector-grow signals
					(+ 1 (vector-length signals))))
	     (vector-set! signals (- (vector-length signals) 1) false)
	     (display "New mutex allocated. Mutex INDEX: ")
	     (display (- (vector-length signals) 1))
	     'ok))
    (define (semaphore-activity m)
      (cond ((eq? m 'acquire) acquire)
	    ((eq? m 'release) release)
	    ((eq? m 'new-signal) (new-signal))
	    ((eq? m 'inspect) inspect)))
    semaphore-activity))









