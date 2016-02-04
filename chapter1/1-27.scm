(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m)
         )
        (else (remainder (* base (expmod base (- exp 1) m)) m))
        )
  )         

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))

  (try-it (+ 1 (random (- n 1))))
  )

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)
        )
  )

(define (square x) (* x x))

(display (fast-prime? 561 1)) (newline)
(display (fast-prime? 1105 1)) (newline)
(display (fast-prime? 1729 1)) (newline)
(display (fast-prime? 2465 1)) (newline)
(display (fast-prime? 2821 1)) (newline)
(display (fast-prime? 6601 1)) (newline)

