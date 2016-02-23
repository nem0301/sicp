(define (fast-expt b n)
  (define (square x) (* x x))
  
  (define (iter base n result)
    (cond ((= n 0) result )
          ((even? n)
           (iter (square base) (/ n 2) result)        
           )
          (else
           (iter base (- n 1) (* base result))
           )
          )
    )
  
  (iter b n 1)  
  )


