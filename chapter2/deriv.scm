(load "test.scm")

; put and get
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
          (else 
            (display k)
            (newline)
            (display (key (car array)))
            (newline)
            (newline)
            (get-helper k (cdr array))
            )
          ))
  (get-helper (list op type) global-array))


; tag and contents
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

; packages
(define (install-sum-package)
  (define (make-sum x y) (list '+ x y))
  (define (addend s) (cadr s))
  (define (augend s) (caddr s))
  (define (deriv-sum s)
    (make-sum (deriv (addend s)) (derive (augend s)))
    )  
  (define (tag x) (attach-tag '+ x))
  (put 'deriv '(+) deriv-sum)
  (put 'make-sum '+
       (lambda (x y) (tag (make-sum x y))))
  'done
  )

(define (make-sum x y)
  ((get 'make-sum '+) x y))

(define (install-product-package)
  (define (make-product x y) (list '* x y))
  (define (multiplier p) (cadr p))
  (define (multiplicand p) (caddr p))
  (define (deriv-product p)
    (make-sum (make-product (multiplier p)
                            (deriv (multiplicand p))) 
              (make-product (deriv (multiplier p)) 
                            (multiplicand p))))   
  (define (tag x) (attach-tag '* x))
  (put 'deriv '(*) deriv-product)
  (put 'make-product '*
       (lambda (x y) (tag (make-product x y))))
  'done
  )

(define (make-product x y)
  ((get 'make-product '*) x y))

; application
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
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (deriv x) (apply-generic 'deriv x))

;(define (deriv exp var)
;  (cond ((number? exp) 0)
;        ((variable? exp)
;         (if (same-variable? exp var) 1 0) 
;         )
;        (else ((get 'deriv (operator exp)) (operands exp) var))     
;        )
;  )

; basic pridicate
(define (=number? exp n)
  (and (number? exp) (= exp n))
  )
(define (variable? exp)
  (symbol? exp)
  )
(define (same-variable? exp1 exp2)
  (and (variable? exp1) (variable? exp2) (eq? exp1 exp2))
  )

(install-sum-package)
(install-product-package)

(define x '((+) x y))

(display (operator x))
(display (operands x))

(define (deploy l)
  (cond ((null? l)
         (newline)
         )
        (else
         (display (cdr (car l)))
         (newline)
         (deploy (cdr l)) 
          )
        )
  )
(newline)
(deploy global-array)

(define (test)
  (testing (list 
;                 (deriv '(+ x 3) 'x)
;                 (deriv '(* x y) 'x)
;                 (deriv '(* (* x y) (+ x 3)) 'x)
;                 (deriv '(** x 12) 'x)
;                 (deriv '(* x y (+ x 3)) 'x)
;                 ((deriv '(x + (3 * (x + (y + 2))))) 'x)
;                 ((deriv '(x + 3 * (x + (y + 2)))) 'x)
;                 ((deriv '(+ x x)) 'x)
                 (get 'deriv (operator x))
;                 ((get 'deriv (operator x)) (operands x))
                 
                        
                 ))
  )













