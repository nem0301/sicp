(load "fast-expt.scm")

(define (div n divisor)
  (define (iter try-exp)
    (if (= 0 (remainder n (fast-expt divisor try-exp)))
        (iter (+ try-exp 1))
        (- try-exp 1)
        )
    )
  
  (iter 1)
  )

(define (cons a b) (* (fast-expt 2 a) (fast-expt 3 b)) )
(define (car z) (div z 2))
(define (cdr z) (div z 3))


(define x (cons 2 2))
