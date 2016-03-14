(load "test.scm")
(load "tag.scm")
(load "putget.scm")
(load "gcd.scm")
(load "deploy.scm")

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

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))


; scheme number package
(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'equ? '(scheme-number scheme-number) =)
  (put '=zero? '(scheme-number) zero?)
  (put 'raise '(scheme-number)
       (lambda (x) (make-rational x 1)))  
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))  
  'done  
  )
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

; rational number package
(define (install-rational-package)
  ; internal procedures
  (define (equ? x y) 
    (= (* (numer x) (denom y)) (* (numer y) (denom x))))
  (define (=zero? rat)
    (zero? (numer rat)))  
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g)) ))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y)))
    )
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y)))
    )
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y)))
    )
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y)))
    )
  
  ; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'equ? '(rational rational) equ?)
  (put '=zero? '(rational) =zero?)
  (put 'raise '(rational)
       (lambda (x) 
         (make-complex-from-real-imag (* (/ (numer x) (denom x)) 1.0) 0)))       
  (put 'add '(rational rational) 
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational) 
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational) 
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational) 
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done  
  )
(define (make-rational n d)
  ((get 'make 'rational) n d))


;complex number package
(define (square x) (* x x))

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

(define (install-complex-package)
  ; imported procedures from rectangular and polar package
  (define (equ? z1 z2)
    (and (= (real-part z1) (real-part z2)) 
         (= (imag-part z1) (imag-part z2))))
  (define (=zero? z)
    (and (zero? (real-part z)) (zero? (imag-part z))))
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))  
  (define (real-part z) (apply-generic 'real-part z))
  (define (imag-part z) (apply-generic 'imag-part z)) 
  (define (magnitude z) (apply-generic 'magnitude z))
  (define (angle z) (apply-generic 'angle z))
  
  ; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  
  ; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'equ? '(complex complex) equ?)
  (put '=zero? '(complex) =zero?)  
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done
  )
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (equ? x y) (apply-generic 'equ? x y)) 
(define (=zero? x) (apply-generic '=zero? x))

(define (raise x) (apply-generic 'raise x))


(install-scheme-number-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)

; test
(define (test)
  (define x1 10)
  (define x2 20)
  (define x3 20)
  (define x4 0)
  (define y1 (make-rational 2 5))
  (define y2 (make-rational 7 13))
  (define y3 (make-rational 7 13))
  (define y4 (make-rational 0 1))
  (define z1 (make-complex-from-real-imag 10 20))
  (define z2 (make-complex-from-real-imag 20 30))
  (define z3 (make-complex-from-real-imag 20 30))
  (define z4 (make-complex-from-real-imag 0 0))
  
  (testing (list (deploy global-array)
                 z1 z2 
                 (add z1 z2)
                 x1 x2
                 (add x1 x2)
                 y1 y2
                 (add y1 y2)
                 (equ? x2 x3)
                 (equ? x1 x3)
                 (equ? y2 y3)
                 (equ? y1 y3)                           
                 (equ? z2 z3)
                 (equ? z1 z3)
                 (=zero? x1)
                 (=zero? x4)
                 (=zero? y1)
                 (=zero? y4)
                 (=zero? z1)
                 (=zero? z4)
                 (raise x1)
                 (raise y1)                 
                 ))
  )
























