(define testlist (list 23 72 149 34))

(define (length a-list)
  (if (null? a-list)
      0
      (+ 1 (length (cdr a-list)))))

(define (last-pair a-list)
  (cond ((null? a-list) (error "List must be non-empty."))
	((= (length a-list) 1) (list (car a-list)))
	(else (last-pair (cdr a-list)))))

;; Weiqun Zhang's solution

(define (wz-last-pair a-list)
  (let ((a (cdr a-list)))
    (if (null? a)
	a-list
	(last-pair a-list))))
