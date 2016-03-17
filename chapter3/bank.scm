(load "test")

(define balance 100)

(define (withdraw amount)
  (if (<= amount balance)
      (begin (set! balance (- balance amount)) balance)
      "Insufficient funds"
      )
  )


(define (test)
  (append-item (withdraw 25))
  (append-item (withdraw 25))
  (append-item (withdraw 60))
  (append-item (withdraw 15))
  
  (deploy results-list)
  )