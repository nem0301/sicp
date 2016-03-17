(load "test")

(define (make-accumulator n)
  (let ((acc n))
    (lambda (x) (begin (set! acc (+ acc x)) acc)))   
  )

(define (test)  
  (define A (make-accumulator 5))
  (append-item (A 10))
  (append-item (A 10))
  (testing results-list)
  )