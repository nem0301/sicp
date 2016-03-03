(load "test.scm")

(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? obj) (eq? (car obj) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left 
        right 
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right)))
  )

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbol-tree x) (caddr x))
(define (weight-tree x) (cadddr x))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (symbol-tree tree)
      )
  )
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (weight-tree tree)
      )
  )

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch) (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)
              )
          )
        )
    )
  
  (decode-1 bits tree)
  )

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit)) 
        )
  )

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set)))
         (cons x set)
         )
        (else (cons (car set) (adjoin-set x (cdr set))))
        )
  )

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) (cadr pair)) 
                    (make-leaf-set (cdr pairs)))        
        )
      )
  )

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree (make-leaf 'B 2)
                                  (make-code-tree (make-leaf 'D 1)
                                                  (make-leaf 'C 1)))
                  )
  )

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(define a (make-leaf 'A 4))
(define b (make-leaf 'B 2))
(define c (make-leaf 'C 1))
(define d (make-leaf 'D 1))
(define e '(A 4))
(define f '(B 2))
(define g '(C 1))
(define h '(D 1))

(define x '())
(define y (list e f g h))

(define (test)
  (testing (list a b c d 
                 (leaf? a)
                 (adjoin-set a x)
                 (adjoin-set b (adjoin-set a x))
                 (adjoin-set c (adjoin-set b (adjoin-set a  x)))
                 (adjoin-set d (adjoin-set c (adjoin-set b (adjoin-set a x))))
                 (make-leaf-set y)
                 (decode sample-message sample-tree)
                 )
           ) 
  )

























