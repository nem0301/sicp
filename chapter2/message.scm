(load "test.scm")
(load "sqrt-with-fixed-point.scm")

(define (square x) (* x x))
(define (apply-generic op arg)
  (arg op)
  )

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))) 
           )
          ((eq? op 'angle) (atan y x))
          (else (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))
          )
    )  
  dispatch)

(define (make-from-mag-ang r a)  
  (define (dispatch op)
    (cond ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          )
    )
  dispatch)


(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2)))
  )

(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2)))
  )

(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2)))
  )

(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2)))
  )

(define (radian-to-degree x)
  (* x 57.2958)
  )

(define z1 (make-from-real-imag 10 20))
(define z2 (make-from-real-imag 20 30))
(define z3 (make-from-mag-ang (sqrt 500) (atan 20 10)))
(define z4 (make-from-mag-ang (sqrt 1300) (atan 30 20)))

(define (test)
  (testing (list "------------"
                 (cons (real-part (add-complex z1 z2))
                       (imag-part (add-complex z1 z2)))
                 (cons (real-part (sub-complex z1 z2))
                       (imag-part (sub-complex z1 z2)))
                 (cons (real-part (mul-complex z1 z2))
                       (imag-part (mul-complex z1 z2)))
                 (cons (real-part (div-complex z1 z2))
                       (imag-part (div-complex z1 z2)))
                 "------------"
                 (cons (real-part (add-complex z3 z4))
                       (imag-part (add-complex z3 z4)))
                 (cons (real-part (sub-complex z3 z4))
                       (imag-part (sub-complex z3 z4)))
                 (cons (real-part (mul-complex z3 z4))
                       (imag-part (mul-complex z3 z4)))
                 (cons (real-part (div-complex z3 z4))
                       (imag-part (div-complex z3 z4)))                
                 "------------"
                 (cons (magnitude (add-complex z1 z2)) 
                       (radian-to-degree (angle (add-complex z1 z2))))
                 (cons (magnitude (sub-complex z1 z2)) 
                       (radian-to-degree (angle (sub-complex z1 z2))))
                 (cons (magnitude (mul-complex z1 z2)) 
                       (radian-to-degree (angle (mul-complex z1 z2))))
                 (cons (magnitude (div-complex z1 z2)) 
                       (radian-to-degree (angle (div-complex z1 z2))))
                 "------------"
                 (cons (magnitude (add-complex z3 z4))
                       (radian-to-degree (angle (add-complex z3 z4))))
                 (cons (magnitude (sub-complex z3 z4)) 
                       (radian-to-degree (angle (sub-complex z3 z4))))
                 (cons (magnitude (mul-complex z3 z4)) 
                       (radian-to-degree (angle (mul-complex z1 z2))))
                 (cons (magnitude (div-complex z3 z4)) 
                       (radian-to-degree (angle (div-complex z3 z4))))     
                 
                 )
           )
  )

