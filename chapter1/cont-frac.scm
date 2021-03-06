(define (cont-frac n d k)
	(define (iter result term)
		(if (= term 0)
			result
			(iter (/ (n term)
					 (+ (d term) result))
				  (- term 1)
			)
		)
	)
	(iter 0 k)
	)