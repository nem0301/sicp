(load "fixed-point.scm")
(load "average.scm")

(define (sqrt x)
	(fixed-point (lambda (y) (average y (/ x y))) 1.0)
	)
