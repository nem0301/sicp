(define (make-segment start-segment end-segment)
  (cons start-segment end-segment)
  )
(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))

(define (make-point x y)
  (cons x y)
  )
(define (point-x p) (car p))
(define (point-y p) (cdr p))

(define (mid-point seg)
  (define (average a b) (/ (+ a b) 2.0))
  (let ((p1 (start-segment seg))
        (p2 (end-segment seg))
        )
    (make-point (average (point-x p2) (point-x p1))
                (average (point-y p2) (point-y p1))
                )
   )  
  )

(define (print-point p)
  (newline)
  (display "(")
  (display (point-x p))
  (display ",")
  (display (point-y p))           
  (display ")")   
  )


(define x 
  (mid-point (make-segment (make-point 2 2) (make-point 10 10))))

(define y 
  (mid-point (make-segment (make-point -10 -12 ) (make-point -3 -3))))

(define z 
  (mid-point (make-segment (make-point -10 -20) (make-point 1 10))))


(define (test)
  (print-point x)
  (print-point y)
  (print-point z)
  )