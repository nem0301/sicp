(load "test")

(define (make-monitored f)
  (define count 0)
  (define (mf m)
    (cond ((eq? m 'how-many-calls?) count)
          ((eq? m 'reset-count) (set! count 0) count)
          (else
           (set! count (+ count 1)) 
           (f m))
          )
    )
  mf)
               


(define (test)
  (define s (make-monitored sqrt))
  (append-item (s 100))
  (append-item (s 100))
  (append-item (s 100))
  (append-item (s 'how-many-calls?))
  (append-item (s 'reset-count))
  (append-item (s 'how-many-calls?))
  (testing results-list)
  )