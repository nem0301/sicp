; "nontrivial square root of 1 modulo n" means that is equal to 1 modulo n
; In other words, it have to come into that a^2 mod n is 1.

(define (square x) (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m)
         )
        (else (remainder (* base (expmod base (- exp 1) m)) m))
        )
  )         

(define (full-fermat-prime? n)
  (define (iter a n)
    (if (= a n) true
      (if (= (expmod a n n) a) 
        (iter (+ a 1) n)
        false
        )
      )
    )
  (iter 1 n)
  )


(define (test-fermat-prime n expected)
  (define (report-result n result expected) 
    (newline) 
    (display n) 
    (display ": ") 
    (display result) 
    (display ": ") 
    (display (if (eq? result expected) "OK" "FOOLED"))) 
  (report-result n (full-fermat-prime? n) expected)
  ) 
