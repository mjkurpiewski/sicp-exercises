(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define a1-wire (make-wire))
(define a2-wire (make-wire))
(define a3-wire (make-wire))
(define a4-wire (make-wire))

(define b1-wire (make-wire))
(define b2-wire (make-wire))
(define b3-wire (make-wire))
(define b4-wire (make-wire))

(define s1-wire (make-wire))
(define s2-wire (make-wire))
(define s3-wire (make-wire))
(define s4-wire (make-wire))

(define carry (make-wire))

(set-signal! a2-wire 1)
(set-signal! a3-wire 1)

(set-signal! b1-wire 1)
(set-signal! b4-wire 1)

(define a-wires (list a1-wire a2-wire a3-wire a4-wire))
(define b-wires (list b1-wire b2-wire b3-wire b4-wire))
(define s-wires (list s1-wire s2-wire s3-wire s4-wire))

(map get-signal s-wires)
(map get-signal a-wires)
(map get-signal b-wires)

(map (lambda (x) (probe (quote x) x)) a-wires)
(map (lambda (x) (probe (quote x) x)) b-wires)
(map (lambda (x) (probe (quote x) x)) s-wires)

(ripple-carry-adder a-wires b-wires s-wires carry)
(map get-signal s-wires)
