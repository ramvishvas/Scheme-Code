;this is the subfunction for computing xor of two bits
(define (xor x y)
	(cond ((= x y) 0)
		(else 1)))

;this is the main function for computing xor of a given list
(define (xor-list ls)
	(cond ((or (null? ls) (null? (cdr ls))) 'invalid-input)
		((null? (cddr ls)) (xor (car ls) (cadr ls)))
		(else (xor (car ls) (xor-list (cdr ls))))))


