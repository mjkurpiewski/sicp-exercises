;; An implementation of sets that requires items to be ordered.
;; In this case, we restrict ourselves to working with 
;; numbers as the elements of the set.

(define (element-of-set? element set)
  (cond ((null? set) false)
	((= element (car set)) true)
	((< element (car set)) false)
	(else (element-of-set? element (cdr set)))))

(define (adjoin-set element set)
  (cond ((null? set) (list element))
	((< element (car set)) (cons element set))
	((= element (car set)) set)
	((> element (car set)) (cons (car set)
				     (adjoin-set element
						 (cdr set))))))
	
(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1))
	    (x2 (car set2)))
	(cond ((= x1 x2)
	       (cons x1 (intersection-set (cdr set1)
					  (cdr set2))))
	      ((< x1 x2) (intersection-set (cdr set1)
					   set2))
	      ((< x2 x1) (intersection-set set1
					   (cdr set2)))))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
	((null? set2) set1)
	(else (let ((x1 (car set1))
		    (x2 (car set2)))
		(cond ((= x1 x2)
		       (cons x1 (union-set (cdr set1)
					   (cdr set2))))
		      ((< x1 x2) 
		       (cons x1 (union-set (cdr set1)
					   set2)))
		      ((< x2 x1) 
		       (cons x2 (union-set set1
					   (cdr set2)))))))))
		
