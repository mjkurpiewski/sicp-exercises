(define m1 (make-mobile (make-branch 2 4)
			(make-branch 4 2)))

(balanced-mobile? m1)

(define m (make-mobile (make-branch 3 5)
		       (make-branch 1 2)))


(define m2 (make-mobile (make-branch 2 m)
			(make-branch 2 m)))


(branch-structure (left-branch m))
(branch-structure (right-branch m))
(not (pair? (branch-structure (left-branch m))))

(and (= (branch-torque (left-branch m))
	(branch-torque (right-branch m))) ;; #f

     (balanced-branch? (left-branch m)) ;; #t
     (balanced-branch? (right-branch m))) ;; #t 
m
(left-branch m)
