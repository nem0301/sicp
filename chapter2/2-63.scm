(load "test.scm")
(define nil '())
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right)
  )

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (entry set)) #t)
        ((< x (entry set)) (element-of-set? x (left-branch set)))
        ((> x (entry set)) (element-of-set? x (right-branch set)))
        )
  )

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x nil nil))
        ((= x (entry set)) set)
        ((< x (entry set)) 
         (make-tree (entry set) 
                    (adjoin-set x (left-branch set)) 
                    (right-branch set))
         )
        ((> x (entry set)) 
         (make-tree (entry set) 
                    (left-branch set)
                    (adjoin-set x (right-branch set)))
         )
        )
  )

(define (tree->list-1 tree)
  (if (null? tree) 
      '()
      (append (tree->list-1 (left-branch tree)) 
              (cons (entry tree) 
                    (tree->list-1 (right-branch tree))))
      )
  )

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree) 
                            (copy-to-list (right-branch tree) result-list)))
        )
    )
  (copy-to-list tree '())
  )

(define (tree->list tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree) 
                            (copy-to-list (right-branch tree) result-list)))
        )
    )
  (copy-to-list tree '())
  )

(define a (make-tree 1 nil nil))
(define b (make-tree 5 nil nil))
(define c (make-tree 11 nil nil))
(define d (make-tree 3 a b))
(define e (make-tree 9 nil c))
(define x (make-tree 7 d e))

(define a (make-tree 1 nil nil))
(define b (make-tree 5 nil nil))
(define c (make-tree 11 nil nil))
(define d (make-tree 9 nil c))
(define e (make-tree 7 b d))
(define y (make-tree 3 a e))

(define a (make-tree 1 nil nil))
(define b (make-tree 7 nil nil))
(define c (make-tree 11 nil nil))
(define d (make-tree 3 a nil))
(define e (make-tree 9 b c))
(define z (make-tree 5 d e))

(define (test)
  (testing (list (tree->list-1 x)
                 (tree->list-2 x)
                 (tree->list-1 y)
                 (tree->list-2 y)
                 (tree->list-1 z)
                 (tree->list-2 z)
            )
           )
  )












