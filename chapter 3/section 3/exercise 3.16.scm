;; Ben Bitdiddle writes a procedure to count the # of pairs in a given list.
;; His program is wrong, why?
;; Show some list structures of 3 pairs that will return:
;; 3, 4, 7, and never return.

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
	 (count-pairs (cdr x))
	 1)))
