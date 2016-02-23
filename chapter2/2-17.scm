(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))
      )
  )

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))
      )
  )

(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))
      )
  )

(define (last-pair x)
  (list-ref x (- (length x) 1))  
  )


(define x '(1 2 3 4))
(define y '(5 6 7 8))

(define (test)
  (newline)
  (display (list-ref x 2))
  (newline)
  (display (length x))
  (newline)
  (display (append x y))
  (newline)
  (display (last-pair x))
  (newline)
  (display (last-pair y))
  )