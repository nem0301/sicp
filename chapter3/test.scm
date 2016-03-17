
(define results-list '())

(define (append-item item) 
  (set! results-list  (append results-list (list item))))


(define (testing cases)
  (cond ((null? cases)
         (newline)
         "succesfully end"
         )
        (else
         (newline)
         (display (car cases))
         (newline)
         (testing (cdr cases)) 
         )     
      )
  )

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

(define (tree-testing cases)
  (define (iter l)
    (cond ((null? (cdr l))
           (newline)
           (display (car l))
           (newline)
           )
          (else
           (newline)
           (display (car l))
           (iter (cdr l))           
           )
        )
    )
  )
