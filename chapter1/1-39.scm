(load "cont-frac.scm")

(define (tan-cf x k)
	(cont-frac (lambda (i)
					(if (= i 1) x
						(- (* x x))
						) )
				(lambda (i)
					(- (* i 2.0) 1) )
				k)
	)