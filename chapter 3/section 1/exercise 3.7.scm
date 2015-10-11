;; Modify the work done in Exercise 3.4 to provide a procedure 'make-joint'
;; 'make-joint' should take three arguments:
;; 1) a password protected account
;; 2) the password for the account
;; 3) a new password
;; 'make-joint' will create an additional access password for the original
;; account. 
(define (make-joint account password joint-password)
  ((account password 'make-joint) joint-password))

(define (make-account balance password)
  (let ((failed-attempts 0)
	(passwords (list password)))
    (define (withdraw amount)
      (if (>= balance amount)
	  (begin (set! balance 
		       (- balance amount))
		 balance)
	  "Insufficient funds"))
    (define (list-pass)
      passwords)
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (new-password joint-password)
      (set! passwords (cons joint-password passwords))
      dispatch)
    (define (call-the-cops)
      (error "Too many incorrect login attempts, the police are on their way."))
    (define (dispatch . m)
      (if (>= failed-attempts 7)
	  (call-the-cops)
	  (if (member (car m) passwords)
	      (cond ((eq? (cadr m) 'withdraw) withdraw)
		    ((eq? (cadr m) 'deposit) deposit)
		    ((eq? (cadr m) 'make-joint) new-password)
		    ((eq? (cadr m) 'debug) (list-pass))
		    (else (error "Unknown request: 
                 MAKE-ACCOUNT" m)))
	      (begin (set! failed-attempts (+ failed-attempts 1))
		     (error "Incorrect password.")))))
    dispatch))
