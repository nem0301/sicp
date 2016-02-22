(load "compose.scm")
(define (square x) (* x x))

(define (repeated proc n)  
  (if (< n 1)
          (lambda (x) x)
          (compose proc (repeated proc (- n 1)) )
          )
  )

(define (test)
  ((repeated square 2) 5)
  )