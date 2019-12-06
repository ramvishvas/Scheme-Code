(define (percentage_above_avg lst1)
        (cond ((null? lst1) "pagal h kya?")))
        
(define (get_marks ls1)
            (cadr ls1))    
                 
(define (list_length ls2)
        (cond ((null? ls2) 0)
              (else (+ 1 (list_length (cdr ls2))))))
              
(define (list_sum ls3)
        (cond ((null? ls3) 0)
              ((null? (cdr ls3)) (car ls3))
              (else (+ (car ls3) (list_sum (cdr ls3))))))
              
(define avg_marks
        (lambda (a b) (/ a b)))

(define (above_avg_std n ls4)
        (cond ((null? ls4) "pagal h kya?")
              ((null? (cdr ls4)) (cond ((< n (car ls4)) 1)
                                       ((else 0))))
              ((< n (car ls4)) (+ 1 (above_avg_std n (cdr ls4))))
              (else (above_avg_std n (cdr ls4)))))
