(load "test.scm")
(load "sqrt-with-fixed-point.scm")

;all trigonometric function's arguments and results are radian

(define (square x) (* x x))

(define (real-part z)
  (cond ((rectangular? z)
         (real-part-rectangular (contents z))
         )
        ((polar? z) 
         (real-part-polar (contents z))
         )
        (else (error "Unknown type -- REAL-PART" z))
        ) 
  )

(define (imag-part z)
  (cond ((rectangular? z)
         (imag-part-rectangular (contents z))
         )        
        ((polar? z)
         (imag-part-polar (contents z))
         )
        (else (error "Unknown type -- IMAG-PART" z))
        ) 
  )

(define (magnitude z)
  (cond ((rectangular? z)
         (magnitude-rectangular (contents z))
         )
        ((polar? z)
         (magnitude-polar (contents z))
         )
        (else (error "Unknown type -- MAGNITUDE" z))
        )
  )

(define (angle z)
  (cond ((rectangular? z)
         (angle-rectangular (contents z))
         )
        ((polar? z)
         (angle-polar (contents z))
         )
        (else (error "Uknown type -- ANGLE" z))
        )  
  )

(define (make-from-real-imag x y) (cons x y))
(define (make-from-mag-ang r a) (cons (* r (cos a)) (* r (sin a))))


; these result of calculation are rectangular result
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

(define (attach-tag type-tag contents)
  (cons type-tag contents)
  )

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)
      )
  )

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)
      )
  )
(define (rectangular? z) (eq? (type-tag z) 'rectangular))
(define (polar? z) (eq? (type-tag z) 'polar))

;Ben's
(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))
(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
           (square (imag-part-rectangular z))
           )) 
  )
(define (angle-rectangular z)
  (atan (imag-part-rectangular z)
        (real-part-rectangular z)) 
  )

(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y))
  )

(define (make-from-mag-ang-rectangular r a)
  (attach-tag 'rectangular
              (cons (* r (cos a)) (* r (sin a))))
  )

;Alyssa's
(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z)))
  )

(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (angle-polar z)))
  )

(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))
(define (make-from-real-imag-polar x y)
  (attach-tag 'polar
              (cons (sqrt (+ (square x) (square y))) (atan y x)))
  )
(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r a))
  )

(define (radian-to-degree x)
  (* x 57.2958)
  )

(define z1 (make-from-real-imag-rectangular 10 20))
(define z2 (make-from-real-imag-rectangular 20 30))
(define z3 (make-from-real-imag-polar 10 20))
(define z4 (make-from-real-imag-polar 20 30))

(define (test)
  (testing (list 
                 (angle z1)
                 
                 "------------"
                 (add-complex z1 z2)
                 (sub-complex z1 z2)
                 (mul-complex z1 z2)
                 (div-complex z1 z2)
                 "------------"
                 (add-complex z3 z4)
                 (sub-complex z3 z4)
                 (mul-complex z3 z4)
                 (div-complex z3 z4)
                 "------------"
                 (cons (magnitude-rectangular (add-complex z1 z2)) 
                       (radian-to-degree (angle-rectangular (add-complex z1 z2))))
                 (cons (magnitude-rectangular (sub-complex z1 z2)) 
                       (radian-to-degree (angle-rectangular (sub-complex z1 z2))))
                 (cons (magnitude-rectangular (mul-complex z1 z2)) 
                       (radian-to-degree (angle-rectangular (mul-complex z1 z2))))
                 (cons (magnitude-rectangular (div-complex z1 z2)) 
                       (radian-to-degree (angle-rectangular (div-complex z1 z2))))
                 "------------"
                 (cons (magnitude-rectangular (add-complex z3 z4)) 
                       (radian-to-degree (angle-rectangular (add-complex z3 z4))))
                 (cons (magnitude-rectangular (sub-complex z3 z4)) 
                       (radian-to-degree (angle-rectangular (sub-complex z3 z4))))
                 (cons (magnitude-rectangular (mul-complex z3 z4)) 
                       (radian-to-degree (angle-rectangular (mul-complex z1 z2))))
                 (cons (magnitude-rectangular (div-complex z3 z4)) 
                       (radian-to-degree (angle-rectangular (div-complex z3 z4))))     
                 
                 )
           )
  )







