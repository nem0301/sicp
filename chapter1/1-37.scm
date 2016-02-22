
;a
(define (cont-frac n d k)
  	(if (= k 0)
       		0
       		(/ (n k) (+ (d k) (cont-frac n d (- k 1))) )
       		)
	)

(define (test-a k)
  	(cont-frac  (lambda (i) 1.0)
              				(lambda (i) 1.0)
              				k)
	)

;b
(define (cont-frac-iter n d k)
  	(define (iter result term)
     		(if (= term 0)
           			result
           			(iter (/ (n term) (+ (d term) result) )
                    (- term 1)
                    			)
           		)
     	)
  	(iter 0 k)
  	)

(define (test-b k)
  	(cont-frac-iter (lambda (i) 1.0)
                  					(lambda (i) 1.0)
                  k)
  	)