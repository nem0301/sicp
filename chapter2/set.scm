(load "test.scm")

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set? x (cdr set)))
        )
  )

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (car set) x) 
         (cons (car set) (adjoin-set x (cdr set)))
         )
        (else
         (cons x set)
         )        
        )
  )

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2)) 
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set (cdr set1) (cdr set2)))
               )
              ((< x1 x2)
               (intersection-set (cdr set1) set2)
               )
              ((< x2 x1)
               (intersection-set set1 (cdr set2))
               )
              )
        )
      )
  )

(define (union-set set1 set2)
  (cond ((and (null? set1) (null? set2)) '())
        ((null? set1) set2)
        ((null? set2) set1)
        (else (adjoin-set (car set1) (union-set (cdr set1) set2)))
        ) 
  )

(define (remove-element-set x set)
  (if (null? set)
      '()
      (if (equal? x (car set))
          (remove-element-set x (cdr set))
          (cons (car set) (remove-element-set x (cdr set)))
          )
      )
  )

(define x '(1 2 3 4))
(define y '(4 5 6 7))
(define z '(1 1 2 2 2 4 5 6))
(define a '(1 2 2 2 3 4 5))

(define (test)
  (testing (list (adjoin-set 3 x)
                 (adjoin-set 5 y)
                 (element-of-set? 4 x)
                 (element-of-set? 3 y)
                 (intersection-set x y)
                 (union-set x y)
                 (remove-element-set 2 z)                 
                 )
           )
  )