(load "list.scm")

;;a
(define (make-mobile left right)
  (list left right)
  )
(define (left-branch x) (list-ref x 0))
(define (right-branch x) (list-ref x 1))


(define (make-branch length structure)
  (list length structure)
  )
(define (branch-length x) (list-ref x 0))
(define (branch-structure x) (list-ref x 1))

;;b
(define (is-mobile? m)
  (if (and (pair? (left-branch m)) (pair? (right-branch m))) #t #f)
  )

(define (is-branch? b)
  (if (number? (branch-length b)) #t #f)
  )

(define (total-weight item)
  (cond ((is-mobile? item)
         (+ (total-weight (left-branch item))
            (total-weight (right-branch item)))
         )
        ((is-branch? item)
         (if (pair? (branch-structure item))
             (total-weight (branch-structure item))
             (branch-structure item)
             )
         )
        )        
  )

;;c
(define (is-balanced? item)
  (cond ((is-mobile? item)
         (if (= (total-weight (left-branch item))
            	(total-weight (right-branch item)))             
             )
         )
        ((is-branch? item))
        )
  )




;;test
(define x (make-branch 1 10))
(define y (make-branch 1 20))

(define mobile-x (make-mobile x y))

(define z (make-branch 1 mobile-x))
(define a (make-branch 1 mobile-x))

(define mobile-y (make-mobile z a))

(define (test)
  (newline)
  (display mobile-y)
  (newline)
  (display (is-mobile? mobile-y))
  (newline)
  (display (total-weight mobile-y))
  )