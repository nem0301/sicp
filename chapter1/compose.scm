(define (inc x) (+ 1 x))
(define (square x) (* x x))

(define (compose f g)
  (lambda (x)
    (f (g x))
    )
  )