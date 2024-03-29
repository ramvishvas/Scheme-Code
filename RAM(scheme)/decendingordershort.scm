
(define maximum      
(lambda (x max)   
(cond ((null? x) max)
      ((> (car x) max) (maximum (cdr x) (car x)))
	  (else (maximum (cdr x) max))
)
)
)
(define sort
(lambda (x)
(cond ((null? x) '())
      (else (cons (maximum x (car x)) (sort (remove x (maximum x (car x)))))
)
)
)
)
(define remove 
	(lambda (x n)
		(cond ((null? x) '())
			  ((= (car x) n) (remove (cdr x) n))
			  (else (cons (car x) (remove (cdr x) n))))))
