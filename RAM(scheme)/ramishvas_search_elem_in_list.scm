(define (srch k list1) 
	  (cond ((null? list1) #f)
	        ((= k (car list1)) #t)
	        (else (srch k (cdr list1)))))
