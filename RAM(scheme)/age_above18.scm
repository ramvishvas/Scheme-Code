;Q: Find the number of people with age above 18.

(define get_list_of_dates
	(lambda (list1)
		(if(null? list1) 
                  '()
		(cons (cadar list1) (get_list_of_dates (cdr list1)))
)
)
)
		      
(define validity_check
	(lambda (ls)
		(cond ((null? ls) 0)
                      ((and (= (cadr ls) 2) (> (car ls) 28)) 0)
                      ((and (< (caddr ls) 2014) (>= (caddr ls) 0)) 
				(if (and (<= (cadr ls) 12) (> (cadr ls) 0) )
                                     (if (and (<= (car ls) 31) (> (car ls) 0))
                                         1
                                         0 
                                     )
                                     0))
                      
                      ((or (<= (cadr ls) 0) (<= (car ls) 0)) 0) 
		      ((and (<= (caddr ls) 2014) (= (cadr ls) 2) (> (caddr ls) 0) (> (car ls) 28)) 0)
                       ((> (caddr ls) 2014) 0)
                       ((and (= (caddr ls) 2014) (= (cadr ls) 9) (<= (car ls) 23)) 1)
                        ((and (= (caddr ls) 2014) (< (cadr ls) 9) (< (car ls) 31)) 1)  
		      (else 0)))) 
                            
(define length
      (lambda (ls) 
 	  (if (null? ls)
            0
              (if (= (validity_check (car ls)) 1)
                  (+ 1 (length (cdr ls)))
                   (length (cdr ls))
)
)
)
)   
(define compare_age
        (lambda (ls)
                (cond ((null? ls) 0)
                      ((< (caddr ls) 1996) 1)
		      ((= 1996 (caddr ls))
                        (if (< (cadr ls) 9)
                         1
                         (if (and (= 9 (cadr ls)) (<= (car ls) 23))
			     1
                             0)
			))
                       (else 0))
)
)
(define above18_count 
      (lambda (ls)
	(cond ((null? ls) 0)
 	      ((= (validity_check (car ls)) 1) 
		(if(= (compare_age (car ls)) 1)
                  (+ 1 (above18_count (cdr ls)))
		   (above18_count (cdr ls))))
		(else (above18_count (cdr ls)))
)
)
)
(define percent_above_18year
        (lambda (list1)
             (if (null? list1)
		0
               (let ((ls (get_list_of_dates list1)))
                        (if (= (length ls) 0) 
			 0
			(* (/ (above18_count ls) (length ls)) 100)))
)
)
)
)	

