(load "fixed-point.scm")

(define (average x y) 
	(/ (+ x y) 2)
	)

(define (sqrt x)
	(fixed-point (lambda (y) (average y (/ x y))) 1.0)
	)