(define (inc x) (+ x 1))

(define (double proc)
  (lambda (x)
    (proc (proc x))     
    )  
  )