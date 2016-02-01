;sum of square of largest two numbers
(define (sum-of-square x y z)
  (cond ( (> x y)
            (if (> y z)
                (+ (* x x) (* y y))
                (+ (* x x) (* z z))
                )
            )
        (else
            (if (> x z)
                (+ (* y y) (* x x))
                (+ (* y y) (* z z))
                )
            )
        )
  )
            
        
