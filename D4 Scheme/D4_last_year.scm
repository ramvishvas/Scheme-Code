;all where bst is age_bst
(define (make-bst age ls rs) (list age ls rs))
(define (get-age bst) (car bst))
(define (get-ls bst) (cadr bst))
(define (get-rs bst) (caddr bst))
(define make-empty-bst '())
(define (is-empty? bst) (null? bst))

(define (search bst val)
   (cond ((not (member? val bst)) '())
         ((= val (get-age bst)) bst)
         ((< val (get-age bst)) (search (get-ls bst) val))
         ((> val (get-age bst)) (search (get-rs bst) val))))

(define (member? x bst)
   (cond ((is-empty? bst) #f)
         ((= x (get-age bst)) #t)
         ((< x (get-age bst)) (member? x (get-ls bst)))
         ((> x (get-age bst)) (member? x (get-rs bst)))))

(define (eldest bst)
   (cond ((is-empty? bst) make-empty-bst)
         ((is-empty? (get-rs bst)) (get-age bst))
         (else (eldest (get-rs bst)))))

(define (youngest bst)
   (cond ((is-empty? bst) make-empty-bst)
         ((is-empty? (get-ls bst)) (get-age bst))
         (else (youngest (get-ls bst)))))

(define (last-left bst x succ)        ;intially succ is null list
   (cond ((= (get-age bst) x) succ)
	 ((< x (get-age bst)) (last-left (get-ls bst) x  bst))
	 (else (last-left (get-rs bst) x succ))))

(define (last-right bst x  pre)        ;intially pre is null list
   (cond ((= (get-age bst) x) pre)
	 ((> x (get-age bst)) (last-right (get-rs bst) x  bst))
	 (else (last-right (get-ls bst) x pre))))

(define (immediate-higher-than-x bst x)
   	(cond ((null? (search bst x)) 'error)
	      ((not (null? (get-rs (search bst x)))) (youngest (get-rs (search bst x))))
	      ((null? (last-left bst x '())) 'successor_does_not_exist)
	      (else (get-age (last-left bst x '())))))

(define (immediate-lower-than-x bst x)
   (cond ((null? (search bst x)) 'error)
	 ((not (null? (get-ls (search bst x)))) (eldest (get-ls (search bst x))))
	 ((null? (last-right bst x '())) 'predecessor_does_not_exist)
	 (else (get-age (last-right bst x '())))))
	 
(define (main1 bst age)
    (cond ((is-empty? (search bst age)) 
           (list (youngest bst) (eldest bst)))
	  (else (list (immediate-lower-than-x bst age)
		      (immediate-higher-than-x bst age)))))

(define (main2 bst age1 age2)
   (cond ((is-empty? bst) 0)
         ((and (> (get-age bst) (min age1 age2)) (< (get-age bst) (max age1 age2)))
          (+ 1 (main2 (get-ls bst) age1 age2) (main2 (get-rs bst) age1 age2)))
         (else (+ (main2 (get-ls bst) age1 age2) (main2 (get-rs bst) age1 age2)))))

