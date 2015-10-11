(load "~/Programming/Scheme/sicp-exercises/chapter 3/section 5/streamprocedures.scm")

(define (show x)
  (display-line x)
  x)

(define x
  (stream-map
   show
   (stream-enumerate-interval 0 10)))


;; Results

#|
x

(user)> (stream-ref x 5)

1
2
3
4
5

5

(user)> (stream-ref x 7)

6
7

7

(user)>
|#
