;it is sqrt procedure that contains cluster of procedures
(define (sqrt x)
  ;average of two arguments
  (define (average x y)
    (/ (+ x y) 2))

  ;if difference between squre of guess and x is less then "0.001" then true, else flase
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))

  ;improve guess value
  (define (improve guess)
    (average guess (/ x guess)))

  
  ;from 1, iterating loop
  (define (sqrt-iter guess)
    (if (good-enough? guess)
      guess
      (sqrt-iter (improve guess))))

  (sqrt-iter 1.0))



