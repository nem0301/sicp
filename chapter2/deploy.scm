(define (deploy l)
  (cond ((null? l)
         (newline)
         )
        (else
         (display (car l))
         (newline)
         (deploy (cdr l))         
          )
        )
  )