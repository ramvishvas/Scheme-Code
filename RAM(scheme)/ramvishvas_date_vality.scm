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
             

