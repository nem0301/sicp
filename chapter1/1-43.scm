(define (square x) (* x x))

(define (repeated proc n)
  (lambda (x)
    (if (= n 1)
        (proc x)
        (proc ((repeated proc (- n 1)) x) )
        ) 
    ) 
  )

(define (test)
  ((repeated square 2) 5)
  )