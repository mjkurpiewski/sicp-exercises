;; {1,3,5,7,9,11} 

(define tree1 (make-tree '7
			 (make-tree '3 
				    (make-tree '1 '() '())
				    (make-tree '5 '() '()))
			 (make-tree '9 
				    '() 
				    (make-tree '11 '() '()))))

(tree->list-1 tree1) ;; (1 3 5 7 9 11)
(tree->list-2 tree1) ;; (1 3 5 7 9 11)

(define tree2 (make-tree 15
			 (make-tree 1 '() '())
			 (make-tree 6
				    (make-tree 5 '() '())
				    (make-tree 10
					       '() 
					       (make-tree 11 '() '())))))

(tree->list-1 tree2) ;; (1 3 5 7 9 11)
(tree->list-2 tree2) ;; (1 3 5 7 9 11)

(define tree3 (make-tree 5
			 (make-tree 3
				    (make-tree 1 '() '())
				    '())
			 (make-tree 9
				    (make-tree 7 '() '())
				    (make-tree 11 '() '()))))

(tree->list-1 tree3) ;; (1 3 5 7 9 11)
(tree->list-2 tree3) ;; (1 3 5 7 9 11)

;; left-size ; 2
;; left-result (partial-tree ele 2)
;; root = 5
;; left-result / pass 2 -> in (1 3) len 2
