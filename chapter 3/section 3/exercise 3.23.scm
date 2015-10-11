(define (make-dequeue)
  (let ((front-ptr '())
	(rear-ptr '()))

    (define (set-front-ptr! item)
      (set! front-ptr item)
      front-ptr)

    (define (set-rear-ptr! item)
      (set! rear-ptr item)
      rear-ptr)

    (define (empty-dequeue?)
      (null? front-ptr))

    (define (front-dequeue)
      (if (empty-dequeue?)
	  (error "FRONT cannot be called with an empty dequeue.")
	  (car front-ptr)))

    (define (rear-dequeue)
      (if (empty-dequeue?)
	  (error "REAR cannot be called with an empty dequeue.")
	  (car rear-ptr)))

    (define (print-dequeue)
      front-ptr)
    
    (define (front-insert-dequeue! item)
      (let ((new-pair (cons item '())))
	(cond ((empty-dequeue?)
	       (set-front-ptr! new-pair)
	       (set-rear-ptr! new-pair)
	       (print-dequeue))
	      (else
	       (set-cdr! front-ptr new-pair)
	       (set-front-ptr! new-pair)
	       (print-dequeue)))))

    (define (rear-insert-dequeue! item)
      (let ((new-pair (cons item '())))
	(cond ((empty-dequeue?)
	       (set-front-ptr! new-pair)
	       (set-rear-ptr! new-pair)
	       (print-dequeue))
	      (else
	       (set-rear-ptr! (cons rear-ptr new-pair))
	       (print-dequeue)))))

    (define (front-delete-dequeue!)
      (cond ((empty-dequeue?)
	     (error "DELETE cannot be called with an empty dequeue."))
	    (else
	     (set-front-ptr! (cdr front-ptr))
	     (print-dequeue))))

    (define (rear-delete-dequeue!)
      (cond ((empty-dequeue?)
	     (error "DELETE cannot be called with an empty dequeue."))
	    (else
	     (set-rear-ptr! (cdr front-ptr))
	     (print-dequeue))))
    
    (define (dispatch m)
      (cond ((eq? m 'print) (print-dequeue))
	    ((eq? m 'empty?) (empty-dequeue?))
	    ((eq? m 'front-insert) front-insert-dequeue!)
	    ((eq? m 'rear-insert) rear-insert-dequeue!)
	    ((eq? m 'front-delete) front-delete-dequeue!)
	    ((eq? m 'rear-delete) rear-delete-dequeue!)
	    ((eq? m 'front) (front-dequeue))
	    ((eq? m 'rear) (rear-dequeue))
	    ((eq? m 'set-front) set-front-ptr!)
	    ((eq? m 'set-rear) set-rear-ptr!)
	    ((eq? m 'front-ptr) front-ptr)
	    ((eq? m 'rear-ptr) rear-ptr)
	    (else
	     (error "Invalid argument:" m))))
    dispatch))






