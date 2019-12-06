;it is gaurenteed that the element whose successor we want to find is in the bst
(define (make-bst val ls rs) (list val ls rs))
(define (get-bst-val bst) (car bst))
(define (get-bst-ls bst) (cadr bst))
(define (get-bst-rs bst) (caddr bst))

(define (minimum bst)
   (cond ((null? bst) 'error)
         ((null? (get-bst-ls bst)) (get-bst-val bst))
         (else (minimum (get-bst-ls bst)))))

(define (last-left bst x succ)        ;intially succ is null list
   (cond ((= (get-bst-val bst) x) succ)
	 ((< x (get-bst-val bst)) (last-left (get-bst-ls bst) x  bst))
	 (else (last-left (get-bst-rs bst) x succ))))


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

(define (successor bst x)
   (cond ((null? (search bst x)) 'error)
	 ((not (null? (get-bst-rs (search bst x)))) (minimum (get-bst-rs (search bst x))))
	 ((null? (last-left bst x '())) 'successor_does_not_exist)
	 (else (get-bst-val (last-left bst x '())))))
	 
	  
		
