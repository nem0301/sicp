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
        ((expt? exp)
         (make-product (make-product (expt exp) (make-expt (base exp) (- (expt exp) 1))) (deriv (base exp) var))
         )
        (else
         (error "unknown expression type -- DERIV" exp)
         )
        )
  )

(define (=number? exp n)
  (and (number? exp) (= exp n))
  )

(define (variable? exp)
  (symbol? exp)
  )

(define (same-variable? exp1 exp2)
  (and (variable? exp1) (variable? exp2) (eq? exp1 exp2))
  )
  
(define (make-sum-list l)
  (if (= (length l) 2)
      (list '+ (car l) (cadr l))
      (make-sum (car l) (make-sum-list (cadr l)))
      )  
  )

(define (make-sum exp1 exp2)
  (cond ((=number? exp1 0) exp2)
        ((=number? exp2 0) exp1)
        ((and (number? exp1) (number? exp2)) (+ exp1 exp2))
        (else (make-sum-list (list exp1 exp2)))
        )
  )
  
(define (make-product-list l)
  (if (= (length l) 2)
      (list '* (car l) (cadr l))
      (make-product (car l) (make-product-list (cadr l)))
      )  
  )

(define (make-product exp1 exp2)
  (cond ((or (=number? exp1 0) (=number? exp2 0)) 0)
        ((=number? exp1 1) exp2)
        ((=number? exp2 1) exp1)
        ((and (number? exp1) (number? exp2)) (* exp1 exp2))        
        (else (make-product-list (list exp1 exp2)))
        )   
  )

(define (make-expt base expt)
  (cond ((=number? base 0) 0)
        ((or (=number? base 1) (=number? expt 0)) 1)
        (else (list '** base expt))
        )
  )

(define (sum? exp)
  (and (pair? exp) (eq? (car exp) '+))
  )

(define (addend exp)
  (cadr exp) 
  )

(define (augend exp)
  (let ((a (cddr exp)))
    (if (= (length a) 1)
        (car a)
        (make-sum-list a)
        )
    )   
  )

(define (product? exp)
  (and (pair? exp) (eq? (car exp) '*))
  )

(define (multiplier exp)
  (cadr exp) 
  )

(define (multiplicand exp)
  (let ((a (cddr exp)))
    (if (= (length a) 1)
        (car a)
        (make-product-list a)
        )
    ) 
  )

(define (expt? exp)
  (and (pair? exp) (eq? (car exp) '**))
  )

(define (base exp)
  (cadr exp)
  )

(define (expt exp)
  (caddr exp)
  )


(define (test)
  (testing (list (deriv '(+ x 3) 'x)
                 (deriv '(* x y) 'x)
                 (deriv '(* (* x y) (+ x 3)) 'x)
                 (deriv '(** x 12) 'x)
                 (deriv '(* x y (+ x 3)) 'x)
                 )) 
  )













