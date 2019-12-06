(define (main1 ls5)
      (let* ((valid_list (valid_name_date ls5)))
            (main2 valid_list)))


(define (main2 valid_list)
      (cond ((null? valid_list) '())
            ((null? (cdr valid_list)) '())
            ((null? (main3 (car valid_list) (cdr valid_list))) (main2 (cdr valid_list)))
            (else (cons (cons (caar valid_list) (main3 (car valid_list) (cdr valid_list))) 
            (main2 (list_b (car valid_list) (cdr valid_list)))))))
            

;input is like (a 2 2 1996) ((b 3 4 1889) (c 9 7 2011)  ......)            
(define (main3 ls6 ls7) 
    (cond ((or (null? ls6) (null? ls7)) '())
          ((eq? (compare_dob (cdr ls6) (cdar ls7)) #f) (main3 ls6 (cdr ls7)))
          (else (cons (caar ls7) (main3 ls6 (cdr ls7))))))
           

;list is like(dd mm yy) (dd mm yy)
(define (compare_dob ls3 ls4)
      (cond ((or (null? ls3) (null? ls4)) #f)
            ((and (= (caddr ls3) (caddr ls4)) (= (cadr ls3) (cadr ls4))
                  (= (car ls3) (car ls4))) #t)
            (else #f)))
            
;ramvishvas         
            
            
(define (list_b ls6 ls7) 
    (cond ((or (null? ls6) (null? ls7)) '())
          ((eq? (compare_dob (cdr ls6) (cdar ls7)) #t) (list_b ls6 (cdr ls7)))
          (else (cons (car ls7) (list_b ls6 (cdr ls7))))))


;ramvishvas        
            
            
            
            
;list is like((name dd mm yyyy) (name dd mm yyyy) .......)
(define (valid_name_date ls2)
     (cond ((null? ls2) '())
           ((eq? (date_vality (cdar ls2)) #t) (cons (car ls2) (valid_name_date (cdr ls2))))
           ((eq? (date_vality (cdar ls2)) #f) (valid_name_date (cdr ls2)))
           (else (valid_name_date (cdr ls2)))))
           
;list is like(dd mm yyyy)
(define (date_vality ls1)
       (cond ((or (< (caddr ls1) 1) (< (cadr ls1) 1) (> (cadr ls1) 12) (< (car ls1) 0) 
                  (> (car ls1) 31)) #f)
             ((and (or (= (cadr ls1) 1) (= (cadr ls1) 3) (= (cadr ls1) 5) (= (cadr ls1) 7) 
                   (= (cadr ls1) 8) (= (cadr ls1) 10) (= (cadr ls1) 12))) #t)
             ((and (or (= (cadr ls1) 4) (= (cadr ls1) 6) (= (cadr ls1) 9) (= (cadr ls1) 11)) 
                   (< (car ls1) 31)) #t)
             ((and (= (cadr ls1) 2)
                   (or (= (remainder (caddr ls1) 400) 0) 
                   (and  (= (remainder (caddr ls1) 4) 0) (not (= (remainder (caddr ls1) 100) 0))))
                   (< (car ls1) 30)) #t)
             ((and (= (cadr ls1) 2) (< (car ls1) 29)) #t)
             (else #f)))
             

;list is like(dd mm yy) (dd mm yy)
(define (compare_dob ls3 ls4)
      (cond ((or (null? ls3) (null? ls4)) #f)
            ((and (= (caddr ls3) (caddr ls4)) (= (cadr ls3) (cadr ls4))
                  (= (car ls3) (car ls4))) #t)
            (else #f)))
            
;list is like((name dd mm yyyy) (name dd mm yyyy) .......)
(define (valid_name_date ls2)
     (cond ((null? ls2) '())
           ((eq? (date_vality (cdar ls2)) #t) (cons (car ls2) (valid_name_date (cdr ls2))))
           ((eq? (date_vality (cdar ls2)) #f) (valid_name_date (cdr ls2)))
           (else (valid_name_date (cdr ls2)))))
           
;list is like(dd mm yyyy)
(define (date_vality ls1)
       (cond ((or (< (caddr ls1) 1) (< (cadr ls1) 1) (> (cadr ls1) 12) (< (car ls1) 0) 
                  (> (car ls1) 31)) #f)
             ((and (or (= (cadr ls1) 1) (= (cadr ls1) 3) (= (cadr ls1) 5) (= (cadr ls1) 7) 
                   (= (cadr ls1) 8) (= (cadr ls1) 10) (= (cadr ls1) 12))) #t)
             ((and (or (= (cadr ls1) 4) (= (cadr ls1) 6) (= (cadr ls1) 9) (= (cadr ls1) 11)) 
                   (< (car ls1) 31)) #t)
             ((and (= (cadr ls1) 2)
                   (or (= (remainder (caddr ls1) 400) 0) 
                   (and  (= (remainder (caddr ls1) 4) 0) (not (= (remainder (caddr ls1) 100) 0))))
                   (< (car ls1) 30)) #t)
             ((and (= (cadr ls1) 2) (< (car ls1) 29)) #t)
             (else #f)))
             

