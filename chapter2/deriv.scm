(load "test.scm")

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0) 
         )
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var))
         )
        ((product? exp)
         (make-sum (make-product (multiplier exp) (deriv (multiplicand exp) var))
                   (make-product (deriv (multiplier exp) var) (multiplicand exp)))
                   
         )
        (else
         (error "unknown expression type -- DERIV" exp)
         )
        )
  )

(define (variable? exp)
  (symbol? exp)
  )

(define (same-variable? exp1 exp2)
  (and (variable? exp1) (variable? exp2) (eq? exp1 exp2))
  )

(define (make-sum exp1 exp2)
  (list '+ exp1 exp2)
  )

(define (make-product exp1 exp2)
  (list '* exp1 exp2) 
  )

(define (sum? exp)
  (and (pair? exp) (eq? (car exp) '+))
  )

(define (addend exp)
  (cadr exp) 
  )

(define (augend exp)
  (caddr exp) 
  )

(define (product? exp)
  (and (pair? exp) (eq? (car exp) '*))
  )

(define (multiplier exp)
  (cadr exp) 
  )

(define (multiplicand exp)
  (caddr exp) 
  )


(define (test)
  (testing (list (deriv '(+ x 3) 'x)
                 (deriv '(* x y) 'x)
                 (deriv '(* (* x y) (+ x 3)) 'x)
                 )) 
  )













