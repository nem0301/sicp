; f(n) = n                                  n < 3
; f(n) = f(n - 1) + 2*f(n - 2) + 3*f(n - 3) n >= 3

(define (f n)
  (if (< n 3)
    n
    (f-iter 2 1 0 n)
    )
  )

(define (f-iter a b c count)
  (if (< count 3)
    a
    (f-iter (calc a b c) a b  (- count 1))
    )
  )

(define (calc a b c)
  (+ a (* 2 b) (* 3 c))
  )
  
