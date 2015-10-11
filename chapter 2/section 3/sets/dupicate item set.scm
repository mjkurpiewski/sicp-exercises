(define (element-of-set? element set)
  (cond ((null? set) false)
	((equal? (car set) element) true)
	(else (element-of-set? element (cdr set)))))

(define (adjoin-set element set)
  (cons element set))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
	((element-of-set? (car set1) set2)
	 (cons (car set1)
	       (intersection-set (cdr set1)
				 set2)))
	(else (intersection-set (cdr set1)
				set2))))

(define (union-set set1 set2)
  (append set1 set2))
