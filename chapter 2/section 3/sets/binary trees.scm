(define (key tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (entry tree) (cadddr tree))

(define (make-tree entry left right key-value)
  (list key-value left right entry))

(define (element-of-set? element set)
  (cond ((null? set) false)
	((= element (entry set)) true)
	((< element (entry set))
	 (element-of-set? element (left-branch set)))
	((> element (entry set))
	 (element-of-set? element (right-branch set)))))

(define (lookup given-key database)
  (let ((this-entry (car database)))
    (cond ((null? database) false)
	  ((= given-key (key this-entry)) (key (this-entry)))
	  ((< given-key (key this-entry))
	   (lookup given-key (left-branch database)))
	  ((> given-key (key this-entry))
	   (lookup given-key (right-branch database))))))

(define (adjoin-set element set)
  (cond ((null? set) (make-tree element '() '()))
	((= element (entry set)) set)
	((< element (entry set))
	 (make-tree (entry set)
		    (adjoin-set element (left-branch set))
		    (right-branch set)))
	((> element (entry set))
	 (make-tree (entry set)
		    (left-branch set)
		    (adjoin-set element (right-branch set))))))

;; Purely recursive
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
	      (cons (entry tree)
		    (tree->list-1 (right-branch tree))))))

;; This one uses a helper function as an iterator
;; This one is better, and closer to O(n) time. 
;; As opposed to tree->list-1, tree->list-2 does not utilize
;; the computationally expensive append function, instead relying
;; on the use of cons only, which runs in about constant time,
;; and we only have to visit each element of the tree once, yielding
;; a time of roughly O(n).
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
	result-list
	(copy-to-list (left-branch tree)
		      (cons (entry tree)
			    (copy-to-list (right-branch tree)
					  result-list)))))
  (copy-to-list tree '()))

(define (list->tree elements)
  (car (partial-tree 
        elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size 
             (quotient (- n 1) 2)))
        (let ((left-result 
               (partial-tree 
                elts left-size)))
          (let ((left-tree 
                 (car left-result))
                (non-left-elts 
                 (cdr left-result))
                (right-size 
                 (- n (+ left-size 1))))
            (let ((this-entry 
                   (car non-left-elts))
                  (right-result 
                   (partial-tree 
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree 
                     (car right-result))
                    (remaining-elts 
                     (cdr right-result)))
                (cons (make-tree this-entry 
                                 left-tree 
                                 right-tree)
                      remaining-elts))))))))

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

(define (binary-union-set set1 set2)
  (let ((list1 (tree->list-2 set1))
	(list2 (tree->list-2 set2)))
    (list->tree (union-set list1 list2))))
