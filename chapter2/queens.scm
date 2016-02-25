(load "map.scm")

(define empty-board nil)

(define (adjoin-position new-row k rest-of-queens)
  (cons new-row rest-of-queens)
  )

(define (safe? k position)
  )



(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter (lambda (positions) (safe? k positions))
                (flatmap (lambda (rest-of-queens)
                           (map (lambda (new-row)
                                  (adjoin-position new-row k rest-of-queens)
                                  )
                                (enumerate-interval 1 board-size))
                           )
                         (queens-cols (- k 1))
                         )
                )
        )
    )
  
  (queens-cols board-size)
  )

(define (test)
  
  (testing (list 
                 ))
  )