;load file that was made by previous ex. 
(load "fermat-test.scm")

(define (smallest-divisor n)
  (find-divisor n 2)
  )

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))
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
  (if (fast-prime? n 100)
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
