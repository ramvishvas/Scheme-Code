(define (append_reverse list1)
    (cond ((null? list1) '( )) ; check is list empty
    ; this will append list1 with reverse
    (else (append (list (car list1)) (append_reverse (cdr list1)) (list (car list1)))))) 
          
;(append_reverse '(1 2 3 4 5 6 7 8 9))   
     
(define (min a b) (if (< a b) a b))
(define (max a b) (if (> a b) a b))          

(define (maxmin l)
	(let loop [(x -inf.0) (y +inf.0) (l l)]
		(cond ((null? l) (list x y))
               (else (loop (max (car l) x) (min (car l) y) (cdr l))))))

; (maxmin '(1 2 3 4 5 6 7 8 9)) 