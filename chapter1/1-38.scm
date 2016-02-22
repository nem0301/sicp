(load "cont-frac.scm")

(define (e-euler k)
  	(+ 2.0 (cont-frac (lambda (i) 1)
                    						(lambda (i)
                            							(if (= (remainder i 3) 2)
                                   	(/ (+ i 1) 1.5) 								
                                   1
                                   )
                            							)
                    						k)
      		)  
	)