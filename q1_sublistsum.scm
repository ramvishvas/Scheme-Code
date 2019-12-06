;this is subfunction for calculating sum of elements of the list
(define (sum-of-elements ls)
	(cond ((null? ls) 0)
		(else (+ (car ls) (sum-of-elements (cdr ls))))))

;this is the main function to compute sum of sublists

(define (sum-of-sublists ls)
	(cond ((null? ls) '())
		(else (cons (sum-of-elements (car ls)) (sum-of-sublists (cdr ls)))))) 


