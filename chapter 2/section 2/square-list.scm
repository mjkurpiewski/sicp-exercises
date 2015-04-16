(define (square x) (* x x))

(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items))
	    (square-list (cdr items)))))

(define (square-list-map items)
  (map square items))

(define (square-list-iter items)
  (define (iter things answer)
    (if (null? things)
	answer
	(iter (cdr things)
	      (cons (square (car things))
		    answer))))
  (iter items '()))

(define (square-list-iter2 items)
  (define (iter things answer)
    (if (null? things)
	answer
	(iter (cdr things)
	      (cons answer
		    (square (car things))))))
  (iter items '()))

(square-list (list 1 2 3 4))
;; returns (1 4 9 16)

(square-list-map (list 1 2 3 4))
;; returns (1 4 9 16)

(square-list-iter (list 1 2 3 4))
;; returns (16 9 4 1)
;; this returns the list in reverse because consing the elements on to the list 
;; places each successive value in the car position, cons = car + cdr, car being 
;; (square x) and cdr being the preexisting list. 

(define x (cons 1 '()))
(define y (cons 4 x))

(square-list-iter2 (list 1 2 3 4))
;; returns ((((() . 1) . 4) . 9) . 16)
