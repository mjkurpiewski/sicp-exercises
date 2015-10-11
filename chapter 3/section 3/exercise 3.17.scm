;; Correct count-pairs to work properly, counting distinct pairs.
;; Hint: Keep an auxiliary data structure that counts pairs
;; that have already been seen.

;; How do we determine whether or not a pair is distinct?
;; What kind of equality should we use? eq? will check for
;; object equality.

;; Reading the Scheme specification tells me that eq? and eqv?
;; will behave the same when testing the equality of pairs.
;; eq? is more computationally efficient, so given a choice between
;; the two, we'll use eq?.

;; So what would be a good algorithm to count distinct pairs?
;; When we find a pair, we can append! it to our internal list of
;; seen/known pairs defined in a let statement, initialized to an empty list.

;; As we traverse the structure passed in to count-pairs, we can count the
;; distinct pairs as we add them/keep a running tally, or, we can take
;; the length of the list of pairs 
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
	 (count-pairs (cdr x))
	 1)))

(define (append! list item)
  (set-cdr! (last-pair list) item)
  list)

(define (last-pair list)
  (if (null? (cdr list))
      list
      (last-pair (cdr list))))

(define (visited-pairs p)
  (let ((visited (list 'z)))
    (begin (append! visited p)
	   visited)))

