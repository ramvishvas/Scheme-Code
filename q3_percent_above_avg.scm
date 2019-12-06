;sutask1 getting marks list
(define (get-marks-list ls)
	(cond ((null? ls) '())
		(else (cons (cadar ls) (get-marks-list (cdr ls))))))

;sumtask2 sum of elemets of a list here it will be used to calculate sum of marks list
(define (sum ls)
	(cond ((null? ls) 0)
		(else (+ (car ls) (sum (cdr ls))))))

;subtask3 to find length of a list
(define (len ls)
	(cond ((null? ls) 0)
		(else (+ 1 (len (cdr ls))))))

;subtask4 to calculate average of a list of numbers
(define (average ls)
	(cond ((null? ls) 0)
		(else (/ (sum ls) (len ls)))))

;subtask5 to count no of students above average ls is marks list and a is average marks
(define (count-above-avg ls a)
	(cond ((null? ls) 0)
		((> (car ls) a) (+ 1 (count-above-avg (cdr ls) a)))
		(else (count-above-avg (cdr ls) a))))

;main finction to calculate % of  students having marks above class average
;input a list of student marks pair
;output % above avg
(define (percent-above-avg ls)
	(cond ((null? ls) 0)
		(else (/ (* (count-above-avg (get-marks-list ls) (average (get-marks-list ls))) 100)
			(len ls)))))
