(load "fixed-point.scm")

(define (logarithm)
	(fixed-point (lambda (x) (/ (log 1000) (log x)) ) 10.0 )		
	)