(define (lg1 ls1)
	   (cond ((null? ls1) "not valid")
	         (else (lg2 ls1))))
(define (lg2 ls2)
          (cond ((null? ls2) 0)
                ((> (car ls2) (lg2 (cdr ls2))) (car ls2))
                (else (lg2 (cdr ls2))))) 
