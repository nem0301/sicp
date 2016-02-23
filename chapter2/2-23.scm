(load "list.scm")

(define (map proc l)
  (if (null? l)
      nil
      (cons (proc (car l)) (map proc (cdr l)))
      )
  )

(define (scale-list l factor)
  (map (lambda (x) (* x factor)) l)
  )

(define (square-list l)
  (map (lambda (x) (* x x)) l)
  )

(define (for-each proc l)
  (let ((l-cdr (cdr l)))
    (proc (car l))
    (if (not (null? l-cdr))
        (for-each proc l-cdr)
        true
        )
    )  
  )


(define x (list 1 2 3 4))

(define (test)
  (newline)
  (display x)
  (newline)
  (display (scale-list x 10))
  (newline)
  (display (square-list x))
  (newline)
  (display (for-each (lambda (x) (* x x)) x) )
  )
