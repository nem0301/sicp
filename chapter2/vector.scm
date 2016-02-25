(load "test.scm")
(define nil (list ))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) 
          (accumulate op initial (cdr sequence)))
      )
  )

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map
                                 (lambda (x) (car x))
                                 seqs))            
            (accumulate-n op init (map
                                   (lambda (x) (cdr x))
                                   seqs)))      
      )
  )

(define (dot-product v w)
  (accumulate + 0 (map * v w))
  )

(define (matrix-*-vector m v)
  (map (lambda (x)
         (dot-product x v)
         ) 
       m)
  )

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x)
           (matrix-*-vector cols x)
           )
         m)
    )
  )

(define (transpose m)
  (accumulate-n cons nil m) 
  )


(define v1 (list 1 1 1))
(define v2 (list 2 2 2))
(define v3 (list 3 3 3))

(define m1 (list v1 v2 v3))
(define m2 (list v3 v2 v1))

(define (test)
  (testing (list (dot-product v1 v2)
                 (matrix-*-vector m1 v2)
                 (matrix-*-matrix m1 m2)
                 (transpose m1)
                 )
           )
  )


