;; sample-input: (main '((a ((1 lg 10) (2 tata 5) (3 sam 7))) (b ((1 lg 2) (2 apple 50) (3 dell 20))) (c ((1 hp 20) (2 tata 2) (3 tata 4) (5 tata 3)))) 'c 'tata)

(define (main ecom-db dept-name manufacturer)
	(minimum (get-required-prod-recs (get-required-dept-prod-recs ecom-db dept-name) manufacturer)))

(define (get-required-prod-recs required-prod-recs manufacturer)
	(cond ((null? required-prod-recs) '())
		((eq? (get-manufacturer (get-first-product required-prod-recs)) manufacturer) 
			(cons (get-first-product required-prod-recs) (get-required-prod-recs (get-rest-product required-prod-recs) manufacturer)))
		(else (get-required-prod-recs (get-rest-product required-prod-recs) manufacturer))))


(define (get-required-dept-prod-recs ecom-db dept-name)
	(cond ((null? ecom-db) '())
		((eq? (get-dept-name (get-first-record ecom-db)) dept-name)
			(get-prod-recs (get-first-record ecom-db)))
		(else (get-required-dept-prod-recs (get-rest-record ecom-db) dept-name))))

(define (make-ecom-db . record)
	record)

(define (get-first-record ecom-db)
	(car ecom-db))

(define (get-rest-record ecom-db)
	(cdr ecom-db))

(define (make-dept-record . record)
	record)

(define (get-dept-name dept-record)
	(car dept-record))

(define (get-prod-recs dept-record)
	(cadr dept-record))

(define (get-first-product prod-recs)
	(car prod-recs))

(define (get-rest-product prod-recs)
	(cdr prod-recs))

(define (make-product-record pid manufacturer price)
	(list pid manufacturer price))

(define (get-pid prod-rec)
	(car prod-rec))

(define (get-manufacturer prod-rec)
	(cadr prod-rec))

(define (get-price prod-rec)
	(caddr prod-rec))

(define (minimum product-list)
	(cond ((null? product-list) '())
		((null? (get-rest-product product-list)) (get-first-product product-list))
		(else 
		(cond ((< (get-price (get-first-product product-list)) (get-price (get-first-product (get-rest-product product-list))))
			(minimum (cons (get-first-product product-list) (get-rest-product (get-rest-product product-list)))))
		      (else (< (get-price (get-first-product product-list)) (get-price (get-first-product (get-rest-product product-list))))
			(minimum (cons (get-first-product (get-rest-product product-list)) (get-rest-product (get-rest-product product-list)))))))))
