(define (delete_max lst)
     (cond ((null? lst) '())
           ((= (max_elm lst) (car lst)) (cdr lst))
           (else (cons (car lst) (delete_max (cdr lst))))))
 
(define (max_elm lst1)
     (cond ((null? lst1) 0)
           ((> (car lst1) (max_elm (cdr lst1))) (car lst1))
           (else (max_elm (cdr lst1)))))
