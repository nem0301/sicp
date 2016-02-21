(load "fixed-point.scm")
(load "average.scm")

(define (golden-ratio)
	(fixed-point (lambda (x) (+ 1 (/ 1 x)) ) 1.0)
	)

(define (avg-golden-ratio)
	(fixed-point (lambda (x) (average x (+ 1 (/ 1 x))) ) 1.0)
	)