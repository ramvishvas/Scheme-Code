(define (make-bst val ls rs) (list val ls rs))
(define (get-bst-val  bst) (car bst))
(define (get-bst-ls bst) (cadr bst))
(define (get-bst-rs bst) (caddr bst))

(define (generate-bst ls)    ;this procedure will create bst with elements in ls inserted in opposite order
  (cond ((null? ls) '())
        (else (insert (car ls)
                      (generate-bst (cdr ls))))))

(define (create-bst ls)
   (generate-bst (reverse ls)))

(define (insert x bst)
   (cond ((null? bst) (make-bst x '() '()))
         ((= x (get-bst-val bst)) bst)
         ((< x (get-bst-val bst))
          (make-bst (get-bst-val bst)
                    (insert x (get-bst-ls bst))
                    (get-bst-rs bst)))
         ((> x (get-bst-val bst))
          (make-bst (get-bst-val bst)
                    (get-bst-ls bst)
                   (insert x (get-bst-rs bst))))))

(define (find-smallest-element bst)
   (cond ((null? bst) 'error)
         ((null? (get-bst-ls bst)) (get-bst-val bst))
         (else (find-smallest-element (get-bst-ls bst)))))

(define (largest bst)
   (cond ((null? bst) 'error)
         ((null? (get-bst-rs bst)) (get-bst-val bst))
         (else (largest (get-bst-rs bst)))))

(define (delete-largest largest bst)
  (cond ((= largest (get-bst-val bst)) (get-bst-ls bst))
        (else (make-bst (get-bst-val bst)
                        (get-bst-ls bst)
                        (delete-largest largest (get-bst-rs bst))))))

(define (find-second-largest bst)
    (largest (delete-largest (largest bst) bst))) 
