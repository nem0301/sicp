(load "identity.scm")
(load "prime.scm")

(define (next x) (+ x 1))

(define (filtered-accumulate combiner null-value term a next b filter)
  (define (iter a result)
    (if (> a b)
      result
      (if (filter a)
        (iter (next a) (combiner (term a) result))
        (iter (next a) result)
        )
      )
    )

  (iter a null-value)
  )

;a
(define (sum-prime a b)
  (display (filtered-accumulate + 0 square a next b prime?))
  )

;b
(define (product-prime n)
  (display (filtered-accumulate * 1 identity 1 next n prime?))
  )
