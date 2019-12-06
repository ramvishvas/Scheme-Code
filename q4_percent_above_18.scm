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
;we will use "case" to overcome this difficulty

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

;subtask to find the age on today(in years) from given dob
;we will take floor (round down) the age for this problem because we have to take age 18 or above
;we are interested in years only
;see this function carefully
(define (age dob)
	(cond ((<= (car dob) 12) (cond ((<= (cadr dob) 9) (- 2015 (caddr dob))) (else (- 2014 (caddr dob)))))
	      (else (cond ((< (cadr dob) 9) (- 2015 (caddr dob)))
			  (else (- 2014 (caddr dob)))))))

;subtask to find list of age
;input is dob list
;output is list of age in years
(define (list-of-age ls)
   (cond ((null? ls) '())
	(else (cons (age (car ls)) (list-of-age (cdr ls))))))


;subtask to count age above or equal to 18
;input is list of age
;output count
(define (count-above-or-equal-to-18 ls)
   (cond ((null? ls) 0)
	((>= (car ls) 18) (+ 1 (count-above-or-equal-to-18 (cdr ls))))
	(else (count-above-or-equal-to-18 (cdr ls)))))

;;subtask to find length of the list
(define (len ls)
	(cond ((null? ls) 0)
		(else (+ 1 (len (cdr ls))))))
;;main function 
(define (percent-above-or-equal-to-18 ls)
   (cond ((null? ls) 0)
	((eq? #f (valid-input-list? (get-dob-list ls))) 'invalid-input)
        (else (* (/ (count-above-or-equal-to-18 (list-of-age (get-dob-list ls)))
		    (len ls))
		100))))



