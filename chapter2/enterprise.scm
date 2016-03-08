(load "test.scm")
(load "putget.scm")
(load "tag.scm")
(load "deploy.scm")

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "No method for these types -- APPLY-GENERIC" 
                 (list op type-tags)))
      )
    )
  )

(define (install-div1-package)
  ; internal procedures  
  (define (get-record file)
    (display file)    
    )
  (define (make-record name address salary)
    (list name address salary)
    )
  
  ; interface to the rest of the system
  (define (tag x) (attach-tag 'div1 x))
  (put 'get-record '(div1) get-record)
  (put 'make-record 'div1
       (lambda (n a s) (tag (make-record n a s))))
  'done   
  )

(define (make-record name address salary)
  (let ((proc (get 'make-record 'div1)))
    (if proc
        (proc name address salary)
        (error "div1" proc)
        )
    )  
  )

(define file '())
(define (insert-record record)
  (set! file (cons record file))
  )

(define (get-record file) 
  (apply-generic 'get-record (car file)))

(display "-------install div1---------") (newline) 
(install-div1-package)
(display "----------------------------") (newline)
(display "---------deploy-------------") (newline)
(deploy global-array)
(display "----------------------------") (newline)

(display "---------insert-------------") (newline)
(insert-record (make-record 'david 'incheon 30000))
(display "----------------------------") (newline)

(display "-----------file-------------") (newline)
(display file)
(newline)
(display "----------------------------") (newline)
(display "--------get-record----------") (newline)
(get-record file) (newline)
(display "----------------------------") (newline)

(define (test)
  (testing (list))
  )