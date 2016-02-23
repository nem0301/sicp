(define (make-interval a b) (cons a b))
(define (lower-bound x) (min (car x) (cdr x)))
(define (upper-bound x) (max (car x) (cdr x)))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))
                 )
  )

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y)))        
        )
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4)
                   )
    )
  )

(define (div-interval x y)
  (if (>= 0 (* (lower-bound y) (upper-bound y)))
      (error "Division error (interval spans 0)" y) 
      (mul-interval x                
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))
                                   )
                    )
      )
  )

(define (sub-interval x y)
  (add-interval x (make-interval (- (upper-bound y)) (- (lower-bound y))))
  )

(define (print-interval x)
  (newline)
  (display (lower-bound x))
  (display " - ")
  (display (upper-bound x))
  )

(define (make-center-width c w)
  (make-interval (- c w) (+ c w))
  )

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2)
  )

(define (width i)
  (/ (- (lower-bound i) (upper-bound i)) 2)
  )


(define x (make-interval 1 4))
(define y (make-interval 4 6))
(define z (make-interval 0 0))

(define (test)
  (print-interval x)
  (print-interval y)

  (print-interval (add-interval x y))
  (print-interval (mul-interval x y))
  (print-interval (div-interval x y))
  (print-interval (sub-interval x y))  
  (print-interval (div-interval x z))
)


