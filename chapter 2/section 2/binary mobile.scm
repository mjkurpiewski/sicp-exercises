;; Given procedures
(define (make-mobile left right) (list left right))
(define (make-branch length structure) (list length structure))

;; Write the following:
;; left and right branch selectors
;; branch-length
;; branch-structure

(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cadr mobile))

(define (branch-length branch) (car branch))
(define (branch-structure branch) (cadr branch))

;; Initial solution to calculating the total weight of a mobile. However, after discussion with a friend,
;; they suggested this procedure would be better rewritten by using mutual recursion, in which one of the
;; two mutually recursive procedures acts as a helper of sorts, accumulating the weights of branches. Thus,
;; the total weight of a mobile could be defined in terms of branch weights. total-weight would then call 
;; branch weight on the left and right branches, and sum them. If the branches are not terminal/leaves, 
;; branch-weight will then call total-weight again, which will occur until it reaches the end of the mobile
;; and then return all those values to total-weight, which will return the total weight of the mobile.

;;(define (total-weight mobile)
 ;; (if (not (pair? (branch-structure mobile)))
   ;;   (branch-structure mobile)
     ;; (+ (total-weight (left-branch mobile))
	;; (total-weight (right-branch mobile)))))

;; Rewritten solution for calculating the total-weight of a mobile
;; Most of the logic from my preceding total-weight function can be applied.

(define (branch-weight branch)
  (if (pair? (branch-structure branch))
      (total-weight (branch-structure branch))
      (branch-structure branch)))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

;; A mobile is said to be balanced if the torque applied by the left branch is equal 
;; to the torque applied by the right branch. In this instance, we define the torque
;; of a branch as the product of its length and its weight.

(define (branch-torque branch)
  (* (branch-weight branch)
     (branch-length branch)))

;; Determining whether or not a mobile is balanced can follow similar mutually recursive pattern
;; like the one found in branch-weight/total-weight. In the former case, the weight of a mobile
;; is recursively defined as the weight of the branches, provided those branches are themselves not
;; mobiles. The same logic applies here: a mobile is balanced, provided the branches of the mobile are
;; balanced, and those branches are themselves not mobiles. 

;; balanced-branch? will call the balanced-mobile? procedure if the branch is a mobile. 
;; if it is a leaf of the mobile, it will return #t. this, however, is only one of the 3
;; conditions required to determine whether or not a mobile is balanced. 

(define (balanced-branch? branch)
  (if (pair? (branch-structure branch))
      (balanced-mobile? (branch-structure branch))
      #t))

;; The actual testing for balance occurs here, in balanced-mobile?. This is where
;; the torques for the branches are calculated, as part of the and predicate which
;; requires balanced torques, as well as #t values returned by balanced-branch?, which
;; tells us that the the branches, of which the torque has been calculated, are terminal
;; and do not consist of submobiles.

(define (balanced-mobile? mobile)
  (and (= (branch-torque (left-branch mobile))
	  (branch-torque (right-branch mobile)))
       (balanced-branch? (left-branch mobile))
       (balanced-branch? (right-branch mobile))))

;; What I would like to do in the pair programming exercise is write an additional function
;; for this program called balance, which takes some mobile as an argument, and balances it
;; by adding 
