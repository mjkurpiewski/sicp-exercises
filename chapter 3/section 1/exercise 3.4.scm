; Modify the work done in Exercise 3.3 to limit the number of incorrect 
; login attempts with a local state variable. When 7 incorrect attempts
; occur, invoke the procedure call-the-cops.
 
(define (make-account balance password)
  (let ((failed-attempts 0))
    (define (withdraw amount)
      (if (>= balance amount)
	  (begin (set! balance 
		       (- balance amount))
		 balance)
	  "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (call-the-cops)
      (error "Too many incorrect login attempts, the police are on their way."))
    (define (dispatch . m)
      (if (>= failed-attempts 7)
	  (call-the-cops)
	  (if (eq? (car m) password)
	      (cond ((eq? (cadr m) 'withdraw) withdraw)
		    ((eq? (cadr m) 'deposit) deposit)
		    (else (error "Unknown request: 
                 MAKE-ACCOUNT" m)))
	      (begin (set! failed-attempts (+ failed-attempts 1))
		     (error "Incorrect password.")))))
    dispatch))
