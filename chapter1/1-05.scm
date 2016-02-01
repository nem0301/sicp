;this code is to explain normal and apprecative order
(define (p) (p) )
(define (test x y)
  (if (= x 0)
    0
    y
    )
  )

(test 0 (p))

;if applicative order, then (p) expand infinitely
;like this,
;(test 0 (p))
;(test 0 (p (p)))
;(test 0 (p (p (p))))
;it is just expression for explain nested procedure not real

;if normal order, then 0 is returned
;like this,
;test 0 (p)) 
;(if (= 0 0) 0 (p)) 
;(if #t 0 (p)) 
;0
