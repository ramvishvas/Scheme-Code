(define (make-bst val ls rs) (list val ls rs))
; (make-bst 4 1 2)
(define (get-val bst) (car bst))
; (get-val '(4 1 2))
(define (get-ls bst) (cadr bst))
; (get-ls '(4 1 2))
(define (get-rs bst) (caddr bst))
; (get-rs '(4 1 2))
(define (is-empty? bst) (null? bst))
;   (is-empty? '(4 1 2))
(define (search bst val )
   	(cond ((not (is-member? val bst)) '())
        	 ((= val (get-val bst)) bst)
         	 ((< val (get-val bst)) (search  (get-ls bst)  val))
        	 ((> val (get-val bst)) (search    (get-rs bst)  val))))
;  (search '(4 (1 () ()) (5 () ())) 5)


(define (is-member? x bst)
	(cond ((is-empty? bst) #f)
		((equal? x (get-val bst)) #t)
		((< x (get-val bst)) (is-member? x (get-ls bst)))
		((> x (get-val bst)) (is-member? x (get-rs bst)))
		(else #f)))

;   (is-member? 5 '(4 (1 () ()) (5 () ())))


(define (is-descendent? x y bst)
	(cond ((is-empty? bst) #f)
	      (else (is-member? x (search bst y)))))


;  (is-descendent? 7 4 '(4 (1 () ()) (5 () (7 () ())))

(define (get-smallest-subtree x y bst)
	(cond   ((is-empty? bst) '())
		((and (< x (get-val bst)) (< y (get-val bst)))
			((get-smallest-subtree x y (get-ls bst))))
		((and (> x (get-val bst)) (> y (get-val bst)))
			((get-smallest-subtree x y (get-rs bst))))
		(else bst)))


;   (get-smallest-subtree 1 5 '(4 (1 () ()) (5 () (7 () ())))
