(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         ;this part of code is problem. expmode is divided by 2 and multiplied by 2
         ;so it is O(n)
         (remainder (* (expmod base (/ exp 2) m)
                       (expmod base (/ exp 2) m))
                    m)
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
