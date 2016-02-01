;pascal triangle
;if n = 0 and k = 0, then 1
;if k = 0, then 1
;if n = k, then 1
;(n k) = (n-1 k-1) + (n-1 k)


(define (pascal n k)
  (if (or (= n 0) (= k 0) (= n k))
    1
    (+ (pascal (- n 1) (- k 1)) (pascal (- n 1) k))
    )
  )

