(load "test")

(define (make-account balance passwd)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount)) balance)
        "Insufficient funds"
        ))
  (define (deposit amount)
    (set! balance (+ balance amount)) 
    balance
    )    
  (define (dispatch pw m)    
    (cond ((not (eq? pw passwd)) 
           (lambda (x) "Incoreect password"))
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          ((eq? m 'balance) balance)
          (else (error "Uknown request - MAKE-ACCOUNT" m))                                      
          )        
    )
  dispatch)

(define (test)
  (define acc (make-account 100 'secret))
  (append-item ((acc 'secret 'withdraw) 40))
  (append-item ((acc 'other 'deposit) 50))  
  
  (testing results-list)
  )