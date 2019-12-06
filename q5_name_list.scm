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

;;subfunction for checking wheather two dobs are same or not
(define (equal-dobs? dob1 dob2)
   (cond ((and (= (car dob1) (car dob2))
	       (= (cadr dob1) (cadr dob2))
		(= (caddr dob1) (caddr dob2))) #t)
	(else #f)))

;;subtask for finding list of names of people having date of birth as dob
;;input a dob and the input list given in the question (that is list of records of people)
(define (list-of-names ls dob)
   (cond ((null? ls) '())
	((equal-dobs? (cadar ls) dob) (cons (caar ls) (list-of-names (cdr ls) dob)))
	(else (list-of-names (cdr ls) dob))))

;;sufunction to output a list whose each item is a list containing the names of the people who have the same birthday
;;given input as people record list and dob list
(define (list-of-name-lists ls dob-list)
   (cond ((null? dob-list) '())
	(else (cons (list-of-names ls (car dob-list)) (list-of-name-lists ls (cdr dob-list))))))

;;memeber function
(define (member? l ls)
	(cond ((null? ls) #f)
		((equal? l (car ls)) #t)
		(else (member? l (cdr ls)))))

(define (remove-duplicate ls)
	(cond ((null? ls) '())
	      ((member? (car ls) (cdr ls)) (remove-duplicate (cdr ls)))
		(else (cons (car ls) (remove-duplicate (cdr ls))))))



;;main function to output a list whose each item is a list containing the names of the people who have the same birthday
;given input as people record list
;important something special is there find in ----(its in else part)
(define (main ls)
   (cond ((null? ls) '())
	((equal? #f (valid-input-list? (get-dob-list ls))) 'invalid-input)
	(else (remove-duplicate (list-of-name-lists ls (get-dob-list ls))))))




	
	
    






;


