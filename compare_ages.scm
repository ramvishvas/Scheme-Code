(define (make-dob dd mm yy)
(list dd mm yy))
(define (get-dd dob)
(car dob))
(define (get-mm dob)
(cadr dob))
(define (get-yy dob)
(caddr dob))

(define (make-person-record name dob)
(list name dob))
(define (get-name person)
(car person))
(define (get-dob person)
(cadr person))

(define (get-firstperson person-log)
(car person-log))
(define (get-restpersons person-log)
(cdr person-log))

(define (youngest person-log small)
(cond ((null? person-log) small)
      ((= (compare (get-firstperson person-log) small) 1) (youngest (get-restpersons person-log) (get-firstperson person-log)))
       (else (youngest (get-restpersons person-log) small))))

(define (compare person1 person2)
 	(cond ((< (get-yy (get-dob person1)) (get-yy (get-dob person2))) 1)
              ((= (get-yy (get-dob person1)) (get-yy (get-dob person2)))
                 (if(< (get-mm (get-dob person1)) (get-mm (get-dob person2)))
                    1
                    (if (= (get-mm (get-dob person1)) (get-mm (get-dob person2)))
		         (if (<= get-dd (get-dob person1) (get-dd (get-dob person2)))
                              1
                              0
                         )
		     0 )
		))
	     (else 0)))

(define (main person-log)
        (if (null? person-log)
            '()
         (get-name (youngest person-log (get-firstperson person-log)))))	
