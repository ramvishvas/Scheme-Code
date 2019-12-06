(define (delete_max lst1)
	  (cond ((null? lst1) '())
	        ((= (car lst1) (maxof lst1)) (cdr lst1))
		  (else (cons (car lst1) (delete_max (cdr lst1))))))	  

(define (maxof lst)
	(cond ((null? lst) 0)
	 ((> (car lst) (maxof (cdr lst)))
		(car lst))
         (else (maxof (cdr lst)))))

;testcases-201508

(delete_max '())

(delete_max '(12))

(delete_max '(45 12 32 21 9 10 23 9))

(delete_max '(5 1200 32 112 91 70 23 100))

(delete_max '(1 2 3 5 7 19 23 90 100))



(maxof '(10))

(maxof '(11 9 8 6 4 3 1))

(maxof '(10 43 55 78 100 40 32))



