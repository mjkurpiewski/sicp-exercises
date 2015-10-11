(define sampletree (list 1 2 (list 3 (list 4 5 6) 7) 8 (list 9 10)))
(define simpletree (list 1 2 (list 3 4 5) 6))

(define (count-leaves x)
  (cond ((null? x) 0)
	((not (pair? x)) 1)
	(else (+ (count-leaves (car x))
		 (count-leaves (cdr x))))))

(define (fringe items)
  (cond ((null? items) '())
	((pair? (car items)) (append (fringe (car items))
				     (fringe (cdr items))))
	(else (append (list (car items))
		      (fringe (cdr items))))))

(define (count-leaves-accumulate t)
  (accumulate + 
	      0
	      (map (lambda (x)
		     (if (not (pair? x))
			 1
			 (count-leaves-accumulate x)))
		   t)))
; (1 2 (3 4 5) 6)
; ((1 2) 3 4 5)
;(define (accumulate op initial sequence)
;  (if (null? sequence)
;      initial
;      (+ (car sequence)
;	  (accumulate op
;		      initial
;		      (cdr sequence)))))
