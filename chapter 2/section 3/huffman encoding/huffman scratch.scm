

(d 1) (c 1) (b 2) (a 4)
we make-code-tree on the first two elements, if they meet the condition that 
weight is <= the next? doesn't sound right. how do we tell if they are the smallest elements. table that for a sec.

({d c} 2) (b 2) (a 4)

(define message-tree
    (generate-huffman-tree '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3) 
			     (YIP 9) (WAH 1))))

(define encoded
    (encode '(Get a job Sha na na na na na na na na 
	      Get a job Sha na na na na na na na na 
	      Wah yip yip yip yip
	      yip yip yip yip yip
	      Sha boom)
	    message-tree))

encoded
(length encoded) ;; 84 bits

;; With a fixed-length code, 3 bits per symbol. The length of the message
;; is 36 symbols, the encoded length would be 108 bits.hahah
