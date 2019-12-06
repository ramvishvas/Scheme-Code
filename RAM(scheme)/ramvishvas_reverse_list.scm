(define (revl list1)
           (cond ((null? list1) '( ))
                 (else (append (revl (cdr list1)) (list (car list1)))))) 
             
             
             ;"OR"    
                 
;(define (revl list1)
 ;          (cond ((null? list1) '( ))
  ;               (else (cons (revl (cdr list1)) (car list1)))))                  
