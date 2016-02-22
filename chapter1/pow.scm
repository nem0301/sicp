(define (pow b p)
  (define (even? x)
    (= (remainder x 2) 0)    
    )
  
  (define (square x) (* x x))
  
  (define (iter res a n)
    (if (= n 0)
        res
        (if (even? n)
            (iter res (square a) (/ n 2))
            (iter (* res a) a (- n 1))
                  
            )
        )        
    )
  
  (iter 1 b p)
  
  )