;"CONSTRUCTOR AND SELECTOR"
;LAVEL 1
(define (make-ecom-db . dept-record) 
        dept-record)

(define (get-first-dept-record ecom-db)
		(car ecom-db))

(define (get-rest-dept-record ecom-db)
		(cdr ecom-db))
;;LAVEL 2
(define (make-dept-recs dept-name prod-recs)
	(list dept-name prod-recs))

(define (get-dept-name dept-recs) 
	(car dept-recs))

(define (get-prod-recs dept-recs) 
	(cadr dept-recs))
;LAVEL 3
(define (prod-recs . prod-record)
	prod-record)

(define (get-first-prod-record  prod-recs)
	(car prod-recs ))

(define (get-rest-prod-record  prod-recs)
	(cdr prod-recs ))
;LEVEL 4
(define (make-prod-record pid  manufacturer price)
	(list pid  manufacturer price))

(define (get-pid prod-record)
	(car prod-record))

(define (get-manufacturer prod-record)
	(cadr prod-record))

(define (get-price prod-record)
	(caddr prod-record))


;SUB FN 1
;(get-required-prod-recs '((1 lg 10) (2 tata 5) (3 lg 7)) 'lg)
(define (get-required-prod-recs prod-recs manufacturer)
	(cond ((null? prod-recs) '())
		  (else (if (equal? manufacturer (cadr (car prod-recs)))
			     	 (cons (car prod-recs) (get-required-prod-recs (cdr prod-recs) manufacturer))
				     (get-required-prod-recs (cdr prod-recs) manufacturer)))))
				   
				   
;SUB FN 2
;(get-required-dept-prod-recs '((a ((1 lg 10) (2 tata 5) (3 sam 7))) (b ((1 lg 2) (2 apple 50) (3 dell 20))) (c ((1 hp 20) (2 tata 2) (3 tata 4) (5 tata 3)))) 'a)


(define (get-required-dept-prod-recs ecom-db dept-name )
	(cond ((null? ecom-db) '())
		(else (if (eq? (car (car ecom-db)) dept-name)
				(cadr (car ecom-db))
				(get-required-dept-prod-recs (cdr ecom-db) dept-name)))))
;SUB FN 3
;(minimum '((1 lg 10) (2 tata 5) (3 sam 7) (2 tata 1)))

 (define (minimum prod-recs)
	(cond ((null? prod-recs) '())
		((null? (get-rest-prod-record prod-recs)) (get-first-prod-record prod-recs))
		(else 
		(cond ((< (get-price (get-first-prod-record prod-recs)) (get-price (get-first-prod-record (get-rest-prod-record prod-recs))))
			       (minimum (cons (get-first-prod-record prod-recs) (get-rest-prod-record (get-rest-prod-record prod-recs)))))
		      (else (minimum (get-rest-prod-record prod-recs)))))))



;(main '((a ((1 lg 10) (2 tata 5) (3 sam 7))) (b ((1 lg 2) (2 apple 50) (3 dell 20))) (c ((1 hp 20) (2 tata 2) (3 tata 4) (5 tata 3)))) 'c 'tata)
;MAIN FN
(define (main ecom-db dept-name manufacturer)
	(minimum (get-required-prod-recs (get-required-dept-prod-recs ecom-db dept-name) manufacturer)))

 
