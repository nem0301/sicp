(load "test.scm")

(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? obj) (eq? (car obj) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (element-of-set? x set) 
   (cond ((null? set) #f) 
         ((equal? x (car set)) #t) 
         (else (element-of-set? x (cdr set))))) 

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

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))
      )
  )

(define (encode-symbol symbol tree)
  (define (branch-correct? branch)
    (if (leaf? branch)
        (equal? symbol (symbol-leaf branch))
        (element-of-set? symbol (symbols branch))
        )
    )
  (let ((lb (left-branch tree))
        (rb (right-branch tree)))
    (cond ((branch-correct? lb)
           (if (leaf? lb) '(0) (cons 0 (encode-symbol symbol lb)))
           )
          ((branch-correct? rb)
           (if (leaf? rb) '(1) (cons 1 (encode-symbol symbol rb)))
           )
          (else (error "bad symbol -- ENCODE-SYMBOL" bit))
          )
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

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs))
  )

(define (successive-merge leaf-set)
  (if (= (length leaf-set) 1)
      (car leaf-set)
      (let ((first (car leaf-set))
            (second (cadr leaf-set))
            (rest (cddr leaf-set)))
        (successive-merge (adjoin-set (make-code-tree first second) rest))
        )
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
                 (encode '(A D A B B C A) sample-tree)
                 (generate-huffman-tree y)
                 (decode sample-message (generate-huffman-tree y))      
                 (encode '(A D A B B C A) (generate-huffman-tree y))
                 )
           ) 
  )

























