(define (%_above_avg_mark list1)
         (cond ((null? list1) 0)
        	  (else (* (/ (above_avg_count (avg (cadr list1)) (cadr list1)) (len (cadr list1))) 100))))
			
(define (above_avg_count n list2)
         (cond ((null? list2) 0)
                  ((> (car list2) n) (+ 1 (above_avg_count n (cdr list2))))
                  (else (above_avg_count n (cdr list2)))))
        
(define (avg list3)
          (cond ((null? list3) 0)
            	   (else (/ (sum  list3) (len list3)))))
            		
(define (sum list4)
            (cond ((null? list4) 0)
            		(else (+ (car list4) (sum (cdr list4))))))
            		
(define (len list5)
         (cond ((null? list5) 0)
                  (else (+ 1 (len (cdr list5))))))
            		
   
