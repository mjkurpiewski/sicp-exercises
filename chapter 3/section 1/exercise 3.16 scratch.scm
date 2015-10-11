(define x (list 'a 'b 'c))
x ; (a b c)
(count-pairs x) ; 3
(define y (list 'a 'b '(c d)))
y ; (a b (c d))
(count-pairs y) ; 5
(define z (list 'a 'b '(c)))
z ; (a b (c))
(count-pairs z) ; 4

(define l1 (list 'a 'a 'a))
l1 ; (a a a)
(count-pairs l1) ;3

(define l2 (list 'a 'a '(a)))
l2 ; (a a (a))
(count-pairs l2) ; 4

(define l3 (list '(a) '(a) '(a)))
l3 ; ((a) (a) (a))
(count-pairs l3)
