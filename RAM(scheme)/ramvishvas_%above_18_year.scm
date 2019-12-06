;input like '((ram mahi yash kk .....) ((dd mm yy) (dd mm yy).......)

(define (%above_18_yr lst1)
       (cond ((null? lst1) 0)
                (else (* (/ (above_18_count (valid_date_list (cadr lst1))) (len (cadr lst1))) 100))))
                
(define (above_18_count lst2)
       (cond ((null? lst2) 0)
                ((eq? (above_18 (car lst2)) #t) (+ 1 (above_18_count (cdr lst2))))
                (else (above_18_count (cdr lst2)))))
                
(define (above_18 lst3)
       (cond ((null? lst3) #f)
             ((> (caddr lst3) 1997) #f)
             ((< (caddr lst3) 1997) #t)
             ((< (cadr lst3) 9)  #t)
             ((= (cadr lst3) 9) (cond ((< (car lst3) 14) #t)
                                      (else #f))
             (else #f)))
                
(define (len lst4)
       (cond ((null? lst4) 0)
                (else (+ 1 (len (cdr lst4))))))
                
;list is like((name dd mm yyyy) (name dd mm yyyy) .......)
(define (valid_date_list ls2)
     (cond ((null? ls2) '())
           ((eq? (date_vality (car ls2)) #t) (cons (car ls2) (valid_date_list (cdr ls2))))
           ((eq? (date_vality (car ls2)) #f) (valid_date_list (cdr ls2)))
           (else (valid_date_list (cdr ls2)))))
           
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
             



