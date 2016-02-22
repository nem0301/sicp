(load "segment.scm")

(define (make-rectangle p1 p2)
  (let ((width (abs (- (point-x p1) (point-x p2))))
        (height (abs (- (point-y p1) (point-y p2)))) 
        )
     (cons width height)   
    )
  )

(define (width rec) (car rec))
(define (height rec) (cdr rec))

(define (perimeter rec)
  (+ (* 2 (width rec)) (* 2 (height rec)))
  )

(define (area rec)
  (* (width rec) (height rec))
  )

(define (print-perimeter-and-area rec)
  (newline)
  (display "perimeter : ")
  (display (perimeter rec))
  (newline)
  (display "area : ")
  (display (area rec)) 
  )


(define x (make-rectangle (make-point 0 0) (make-point 10 10)))
(define y (make-rectangle (make-point -10 -10) (make-point -2 -2)))
(define z (make-rectangle (make-point -10 -10) (make-point 10 10)))

(define (test)
  (print-perimeter-and-area x)
  (print-perimeter-and-area y)
  (print-perimeter-and-area z)
  )