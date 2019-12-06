;it is gaurenteed that the element whose perdecessor we want to find is in the bst
(define (make-bst val ls rs) (list val ls rs))
(define (get-bst-val bst) (car bst))
(define (get-bst-ls bst) (cadr bst))
(define (get-bst-rs bst) (caddr bst))

(define (maximum bst)
   (cond ((null? bst) 'error)
         ((null? (get-bst-rs bst)) (get-bst-val bst))
         (else (maximum (get-bst-rs bst)))))

(define (last-right bst x  pre)        ;intially pre is null list
   (cond ((= (get-bst-val bst) x) pre)
	 ((> x (get-bst-val bst)) (last-right (get-bst-rs bst) x  bst))
	 (else (last-right (get-bst-ls bst) x pre))))


(define (search bst val)
   (cond ((not (member? val bst)) '())
         ((= val (get-bst-val bst)) bst)
         ((< val (get-bst-val bst)) (search (get-bst-ls bst) val))
         ((> val (get-bst-val bst)) (search (get-bst-rs bst) val))))

(define (member? x bst)
   (cond ((null? bst) #f)
         ((= x (get-bst-val bst)) #t)
         ((< x (get-bst-val bst)) (member? x (get-bst-ls bst)))
         ((> x (get-bst-val bst)) (member? x (get-bst-rs bst)))))

(define (predecessor bst x)
   (cond ((null? (search bst x)) 'error)
	 ((not (null? (get-bst-ls (search bst x)))) (maximum (get-bst-ls (search bst x))))
	 ((null? (last-right bst x '())) 'predecessor_does_not_exist)
	 (else (get-bst-val (last-right bst x '())))))
	 
	  
		
