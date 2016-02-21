(load "fixed-point.scm")
(load "average-damp.scm")

(define (square x) (* x x))

(define (cube-root x)
	(fixed-point (average-damp (lambda (y) (/ x (square y)) )) 1.0)
	)