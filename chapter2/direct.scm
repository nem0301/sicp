(load "test.scm")
(load "sqrt-with-fixed-point.scm")
(define (square x) (* x x))

(define global-array '())

(define (make-entry k v) (list k v))
(define (key entry) (car entry))
(define (value entry) (cadr entry))

(define (put op type item)
  (define (put-helper k array)
    (cond ((null? array) (list(make-entry k item)))
          ((equal? (key (car array)) k) array)
          (else (cons (car array) (put-helper k (cdr array))))))
  (set! global-array (put-helper (list op type) global-array)))

(define (get op type)
  (define (get-helper k array)
    (cond ((null? array) #f)
          ((equal? (key (car array)) k) (value (car array)))
          (else (get-helper k (cdr array)))))
  (get-helper (list op type) global-array))

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


;Ben's
(define (install-rectangular-package)
  ;internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done  
  )

;Alyssa
(define (install-polar-package)
  ;internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y))) (atan y x)))
  ;interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done  
  )

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "No method for these types -- APPLY-GENERIC" 
                 (list op type-tags)))
      )
    )
  )

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))
(define (make-from-real-imag x y)
  (let ((proc (get 'make-from-real-imag 'rectangular)))
        (if proc
            (proc x y)
            (error "rect" proc) 
            )        
    )
  )
(define (make-from-mag-ang r a)
  (let ((proc (get 'make-from-mag-ang 'polar)))
        (if proc
            (proc r a)
            (error "polar" proc) 
            )            
    )  
  )


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

(install-rectangular-package)
(install-polar-package)

(define z1 (make-from-real-imag 10 20))
(define z2 (make-from-real-imag 20 30))
(define z3 (make-from-mag-ang (sqrt 500) (atan 20 10)))
(define z4 (make-from-mag-ang (sqrt 1300) (atan 30 20)))

(define (test)
  (testing (list "------------"
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

