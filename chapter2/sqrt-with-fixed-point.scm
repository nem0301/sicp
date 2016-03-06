(load "fixed-point.scm")
(load "average-damp.scm")

(define (sqrt x)
	(fixed-point (average-damp (lambda (y) (/ x y))) 1.0)
	)