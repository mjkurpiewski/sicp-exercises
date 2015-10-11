(define sum 0)

(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq
  (stream-map
   accum
   (stream-enumerate-interval 1 20)))

(define y (stream-filter even? seq))

(define z
  (stream-filter
   (lambda (x)
     (= (remainder x 5) 0)) set))

#|
Flow of execution will be along the lines for

a) (stream-ref y 7)
Looking for the 7th element of the stream y,
which is itself a stream which filters
the stream seq for even values. 

Seq itself is a mapping of a mutating accumulator.

The evaluation of seq will proceed as follows:

1   / sum 1   // not even
3   / sum 3   // not even
6   / sum 6   // even! (1 of 7)
10  / sum 10  // even! (2 of 7)
15  / sum 15 // odd
21  / sum 21 // odd
28  / sum 28 // even! (3 of 7)
36  / sum 36 // even! (4 of 7)
45  / sum 45 // odd
55 / sum 55 // odd
66 / sum 66 // even! (5 of 7)
78 / sum 78 // even! (6 of 7)
91 / sum 91 // odd
105 / sum 105 / odd
120 / sum 120 // even! (7 of 7)
136 / sum 136 // even! - index of 7
