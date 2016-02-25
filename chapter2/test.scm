(define (testing cases)
  (cond ((null? cases)
         (newline)
         "succesfully end"
         )
        (else
         (newline)
         (display (car cases))
         (testing (cdr cases)) 
         )     
      )
  )