; find length of list
(define (len lst) 
	(cond ((null? lst) 0)
		(else (+ 1 (len (cdr lst))))))

; if first match found check rest is matching or not
; if rest is matching return 1
; else return 0
(define (match? lst1 lst2 l)
	(cond ((equal? l 0) 1)
		((not (equal? (car lst1) (car lst2))) 0)
		(else (match? (cdr lst1) (cdr lst2) (- l 1)))))

; if match is found discard lst1 from lst2
; and return remaining lst2
(define (A-B lst l)
	(cond ((equal? 0 l) lst)
		(else (A-B (cdr lst) (- l 1)))))

; count occurence of lst1 into lst2
(define (count-match lst1 lst2)
	(cond ((or (null? lst1) (null? lst2)) 0)
		((> (len lst1) (len lst2)) 0)
		; if first match found
		((equal? (car lst1) (car lst2)) 
			; check rest is matching ot not
			; if rest is not matching
			(cond ((equal? (match? lst1 lst2 (len lst1) 0) (count-match lst1 (cdr lst2))))
				; if rest is matching
				; then count and distacd lst1 from lst2
				(else (+ 1 (count-match lst1 (A-B lst2 (len lst1))))))
		(else ((count-match lst1 (cdr lst2))))))

(count-match '(1 2 3) '(1 2 3 4 5 1 2 3 4 5 1 2 3))