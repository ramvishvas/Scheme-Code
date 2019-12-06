
Question: Find the differnce between two sets.

;compare is used to check wheter an element is present in the list or not
;input (compare (4 2 3) 4) output: 1
(define (compare ls1 x)
(cond
 
   ((null? ls1) 0)
   ((= (car ls1) x) 1)
   (else (compare (cdr ls1) x))
 )

)

;remove is used to remove an element from the list 
;input (remove '(1 2 3) 1) output: (2 3) 
(define (remove ls n)
 (cond 
   
    ((null? ls) ())
    ((not (= (car ls) n)) (cons (car ls) (remove (cdr ls) n)))
    (else (remove (cdr ls) n))
 )
)
;set_difference is to print the final difference of sets
;input:(set_difference '(1 2 3) '(1 2 4)) output:(3)
 
  (define  (set_difference ls1 ls2)
  (cond 
    ((null? ls1) '())
    ((null? ls2) ls1)
    ((= (compare ls1 (car ls2)) 1) 
     (set_difference (remove ls1 (car ls2)) (cdr ls2 ))) 
 (else (set_difference ls1 (cdr ls2)) ) 
)
)
