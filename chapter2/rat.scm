(load "gcd.scm")

(define (make-rat n d)
  (let ((g ((if (< d 0) - +) (gcd n d))))
    (cons (/ n g) (/ d g))
    )
  )

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))
            )
  )
 
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))
            )
  )

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))
            )
  )

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))
            )
  )

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (denom x) (numer y))
     )
  )

(define (print-rat rat)
  (newline)
  (display (numer rat))
  (display "/")
  (display (denom rat))
  )

(define x (make-rat 1 2))
(define y (make-rat -2 -4))
(define z (make-rat -1 2))
(define a (make-rat 2 -4))

(define (test)
  (print-rat x)
  (print-rat y)
  (print-rat z)
  (print-rat a)
  )



