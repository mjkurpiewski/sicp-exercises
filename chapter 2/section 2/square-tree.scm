;; This is a procedure that takes as argument a tree in the 
;; form of a list. It maintains the tree structure of the
;; list and squares each element in the tree.

(define testtree (list 1 (list 2 (list 3 4) 5) (list 6 7)))

;; Without higher-order procedures
(define (square-tree tree)
  (cond ((null? tree) '())
	((not (pair? tree)) (square tree)) 
	(else (cons (square-tree (car tree))
		    (square-tree (cdr tree))))))

;; Implemented in terms of map and a lambda procedure.
(define (square-tree-map tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (square-tree-map sub-tree)
	     (square sub-tree)))
       tree))

;; Captured the more abstract process of mapping
;; over a tree by some given procedure.
(define (tree-map procedure tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (tree-map procedure sub-tree)
	     (procedure sub-tree)))
       tree))

;; (tree-map cube testtree)  => (1 (8 (27 64) 125) (216 343))
;; (tree-map square testtree) => (1 (4 (9 16) 25) (36 49))

(define (cube x) (* x x x))
