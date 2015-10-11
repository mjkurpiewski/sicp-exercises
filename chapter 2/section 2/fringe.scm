;; What is the procedure fringe?
;; Fringe takes one piece of data, a list.
;; Fringe returns another list with the same elements of the input list
;; but sublists of the list are stripped, and the elements are also
;; arranged in the same order.

;; This will probably use some of the logic/structure found in deep-reverse.scm

;; This is the example of output from fringe as given in the book:
;; (define x (list (list 1 2) (list 3 4)))
;; (fringe x) -> (1 2 3 4)
;; (fringe (list x x)) -> (1 2 3 4 1 2 3 4)
;;   (list x x) : (((1 2) (3 4)) ((1 2) (3 4)))

(define x (list (list 1 2) (list 3 4)))


(define (fringe items)
  (cond ((null? items) '())
	((pair? (car items)) (append (fringe (car items))
				     (fringe (cdr items))))
	(else (append (list (car items))
		      (fringe (cdr items))))))
