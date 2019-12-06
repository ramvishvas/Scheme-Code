(define (validity_checker lst1)
       (cond ((null? lst1) 1)
              ((> (cadr (car lst1)) 31) 0)
 	      ((> (caddr (car lst1)) 12) 0)	
              (else (validity_checker (cdr lst1)))))

(define comparator
	(lambda (ls max)
		(cond ((null? ls) (car max))
			  ((< (cadddr (car ls)) (cadddr max)) (comparator (cdr ls) (car ls)))
			  ((< (caddr (car ls)) (caddr max)) (comparator (cdr ls) (car ls)))
			  ((< (cadr (car ls)) (cadr max)) (comparator (cdr ls) (car ls)))
			  (else (comparator (cdr ls) max))))) 
(define eldest
    (lambda (lst1)
    (cond ((null? lst1) (display "invalid input"))
	  ((= (validity_checker lst1) 0) (display "check input"))
	  (else (comparator lst1 (car lst1))))))
