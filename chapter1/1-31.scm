; a
(load "identity.scm")
(define (product term a next b)
  (if (> a b)
    1
    (* (term a)
       (product term (next a) next b))
    )
  )

(define (next x) (+ x 1))

(define (factorial n)
  (product identity 1 next n)
  )

(define (pi-term n)
  (if (even? n)
    (/ (+ n 2) (+ n 1))
    (/ (+ n 1) (+ n 2))
    )
  )

; b

(define (product2 term a next b)
  (define (iter a result) 
    (if (> a b)
      result
      (iter (next a) (* (term a) result))
      )
    )
  (iter a 1)
  )
    
