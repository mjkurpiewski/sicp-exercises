(define (make-table)
  (list '*table*))

(define (lookup key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
	(let ((record (assoc lkey-2 (cdr subtable))))
	  (if record
	      (cdr record)
	      false))
	false)))

(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
	(let ((record (assoc key-2 (cdr subtable))))
	  (if record
	      (set-cdr! record value)
	      (set-cdr! subtable
			(cons (cons key-2 value)
			      (cdr subtable)))))
	(set-cdr! table
		  (cons (list key-1
			      (cons key-2 value))
			(cdr table)))))
  'ok)

;;; Below this point, analogous code for a table system that handles n key values.
;;; Roughly, the idea will be to iterate over the list of keys recursively,
;;; where each subsequent iteration goes one key deeper in to the list.
;;; If at any point, one of the keys in the chain is not found, the program will return false.
;;; Insert should work similarly, iterating through a list of keys, using a helper function,
;;; until we come to the end of the key list. If there is an extant key, we replace the value
;;; with the value of 'value' passed into the function. If there is not, we create a new record.

(define (make-n-table)
  (list '*table*))

(define (lookup-n table . keys)
  (define (lookup-n-iterator table keys)
    (let ((subtable (assoc (car keys) (cdr table))))
      (cond ((and subtable (not (null? (cdr keys))))
             (lookup-n-iterator subtable (cdr keys)))

            ((and subtable (null? (cdr keys)))
             (cdr subtable))

            (else false))))
  (lookup-n-iterator table keys))

(define (insert-n! table value . keys)
  (define (insert-n-iterator! table keys value)
    (let ((subtable (assoc (car keys) (cdr table))))
      (cond ((and subtable (not (null? (cdr keys))))
             (insert-n-iterator! subtable (cdr keys) value))

            (subtable
             (set-cdr! subtable value))

            ((not subtable)
             (set-cdr! table (cons (cons (car keys) value)
                                      (cdr subtable))))

            ((and subtable (null? (cdr keys)))
             ((set-cdr! subtable (cons (cons (car keys) '())
                                       (cdr subtable)))
              (insert-n-iterator! subtable (cdr keys) value)))
      )))
  (insert-n-iterator! table keys value)
  'ok)
