(define (smallest-divisor n)
  (find-divisor n 2)
  )

(define (find-divisor n test-divisor)
  (define (next n)
    (if (= n 2) 3 (+ n 2))
    )

  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))
        )
  )

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n))
  )

(define (square x) (* x x))

(define (timed-prime-test n)
  (start-prime-test n (runtime))
  )

(define (start-prime-test n start-time)
  (if (prime? n)
    (report-prime n (- (runtime) start-time))
    )
  )

(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time)
  )

(timed-prime-test 950000000003)

;it takes 3.5 before apply next
;it takes 2.3 after apply next

;the ratio of the speed of the two algorithm is not 2, rougly 1.5
;because extra 'if' test takes time in next procedure
