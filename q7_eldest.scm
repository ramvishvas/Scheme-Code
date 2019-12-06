;subtask1 to get list of dobs from the given record list
(define (get-dob-list ls)
	(cond ((null? ls) '())
		(else (cons (cadar ls) (get-dob-list (cdr ls))))))


;subtask to check validity of one dob suppose today's date is (12 9 2015)
;this is important function pay attention
;(define (vaild-dob? dob)
	;(cond ((and (and (>= (car dob) 1) (<= (car dob) 31)) 
		   ;(and (>= (cadr dob) 1) (<= (cadr dob) 12))
		   ;(and (>= (caddr dob) 1) (<= (caddr dob) 2015))) #t)
		;(else #f)))

;note that valid-dob? function is wrong as it can have 31 days in all the months which is not possible
;we will use case to overcome this difficulty

(define (valid-dob? dob)
	(cond ((and (>= (caddr dob) 1)
		   (and (>= (cadr dob) 1) (<= (cadr dob) 12))
 		   (case (cadr dob)
		      ((1 3 5 7 8 10 12) (and (>= (car dob) 1) (<= (car dob) 31)))
		      ((2) (and (>= (car dob) 1) (<= (car dob) 28)))
		      (else (and (>= (car dob) 1) (<= (car dob) 30)))))  #t)
		(else #f)))

; subtask to find that dob is in past as compared to today or not
;this function check that wheather any future dob is there in the list

(define (dob-past? dob)
	(cond ((< (caddr dob) 2015) #t)
	      ((> (caddr dob) 2015) #f)
		(else (cond ((< (cadr dob) 9) #t)
			    ((> (cadr dob) 9) #f)
			    (else (cond ((<= (car dob) 12) #t)
					(else #f)))))))

;subtask to check validity of dob-list
;input here is dob list
;subtask
(define (valid-input-list? ls)
	(cond ((null? ls) #t)
		((eq? #f (and (dob-past? (car ls)) (valid-dob? (car ls)))) #f)
		(else (valid-input-list? (cdr ls)))))

;;subtask to find elder given two dobs
(define (elder dob1 dob2)
	(cond ((> (caddr dob1) (caddr dob2)) dob2)
	      ((< (caddr dob1) (caddr dob2)) dob1)
		(else (cond ((> (cadr dob1) (cadr dob2)) dob2)
			    ((< (cadr dob1) (cadr dob2)) dob1)
			    (else (cond ((>= (car dob1) (car dob2)) dob2)
					(else dob1)))))))

;get-name subfunction to find name corresponding to dob
;it is gaureented that dob is there in the list
(define (get-name dob ls)
   (cond ((equal? dob (cadar ls)) (caar ls))
	  (else (get-name dob (cdr ls)))))


;;main2 function to find eldest
;input--people record,,output--dob of the eldest
(define (main2 ls)
	(cond ((null? ls) 'invaid-input)
		((null? (cdr ls)) (cadar ls))
		((null? (cddr ls)) (elder (cadar ls) (cadadr ls)))
		(else (elder (cadar ls) (main2 (cdr ls))))))

;main function to find the name of eldest
(define (main ls)
   (get-name (main2 ls) ls))
		





