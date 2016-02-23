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

(define (square-list-2 l)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
;; This new-and-not-improved version conses the answer to the squared 
;; value, but the answer is a list, so you'll end up with (list (list 
;; ...) lastest-square).              
              (cons answer (square (car things)))
              )
        )
    )
  (iter l nil)
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
  (display (square-list-2 x))
  )
