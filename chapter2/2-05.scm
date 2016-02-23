(define (exp base n)
  (define (square x) (* x x))
  
  (define (iter x result)
    (if (= 0 x)
        result
        (if (= (remainder x 2) 0)
            (iter (/ x 2) (square result))            
            (iter (- x 1) (* base result))
        )
    )
  (iter n 1)
  )