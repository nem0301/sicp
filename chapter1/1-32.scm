(load "identity.scm")
(define (next x) (+ x 1))

(define (factorial n)
  (accumulate-iter * 1 identity 1 next n)
  )

(define (pi-term n)
  (if (even? n)
    (/ (+ n 2) (+ n 1))
    (/ (+ n 1) (+ n 2))
    )
 )


;a
(define (accumulate combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a) (accumulate combiner null-value term (next a) next b))
    )
  )
  

;b
(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (combiner (term a) result))
      )
    )

  (iter a null-value)
  )
