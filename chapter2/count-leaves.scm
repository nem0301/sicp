(load "map.scm")

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else
         (+ (count-leaves (car x))
            (count-leaves (cdr x))
            ) 
          )
        )
  )

(define (count-leaves2 tree)
  (accumulate + 
              0
              (map (lambda (x) 
                     (if (pair? x) 0 1 )
                     ) 
                   (enumerate-tree tree)))
  )

(define (count-leaves3 tree)
  (accumulate + 
              0
              (map (lambda (x) 
                     (if (pair? x) 
                         (count-leaves3 x) 
                         1 )
                     ) 
                   tree))
  )

(define x (cons (list 1 2) (list 3 4)))

(define (test)
  (newline)
  (display (count-leaves (list (list x x) (list x x))))
  (newline)
  (display (count-leaves2 (list (list x x) (list x x))))
  (newline)
  (display (count-leaves3 (list (list x x) (list x x))))
  )