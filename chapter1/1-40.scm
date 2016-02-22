(load "newton-transform.scm")

(define (square x) (* x x))
(define (cube x) (* x x x))
  
(define (cubic a b c)
  (lambda (x)
    (+ (cube x)
       (* (square x) a)
       (* x b)
       c)
    )
  )


(define (test)
  (newton-method (cubic 2 2 2) 1.0)
  )