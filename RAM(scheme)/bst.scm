(define (make-tree entry left right)
(list entry left right))

(define empty '())       

(define (is-empty-bst? bst)
(if (null? bst)
#t
#f)
)

(define (get-entry bst)
(car bst))

(define (get-leftsubtree bst)
(cadr bst))

(define (get-rightsubtree bst)
(caddr bst))

(define (is-leaf? bst)
(and (is-empty-bst? (get-leftsubtree bst))  (is-empty-bst? (get-rightsubtree bst))))

(define (insert x bst)
(cond ((is-empty-bst? bst) (make-tree x empty empty))
      ((equal? x (get-entry bst)) bst)
      ((< x (get-entry bst) ) (make-tree (get-entry bst) (insert x (get-leftsubtree bst)) (get-rightsubtree bst)))
      (else (make-tree (get-entry bst) (get-leftsubtree bst) (insert x (get-rightsubtree bst))))
)
) 

(define (binarysearchtree lst bst)
(cond ((is-empty-bst? lst) bst)
      (else (binarysearchtree (cdr lst) (insert (car lst) bst)))
)
)





(define (search bst x)
 (cond ((is-empty-bst? bst) #f)
         ((equal? x (get-entry bst)) #t)
         ((< x (get-entry bst)) (search (get-leftsubtree bst) x))
         (else (search (get-rightsubtree bst) x))))

(define (tree->lst bst)
(cond ((is-empty-bst? bst) empty)
      (else (append (tree->lst (get-leftsubtree bst)) (cons (get-entry bst) (tree->lst (get-rightsubtree bst)))))))   

(define (searchlst lst x)
((lambda (bst) 
(cond ((is-empty-bst? bst) #f)
         ((equal? x (get-entry bst)) #t)
         ((< x (get-entry bst)) (search (tree->lst (get-leftsubtree bst)) x))
         (else (search (tree->lst (get-rightsubtree bst)) x)))) (binarysearchtree lst '())) ) 

(define (deletion bst x)
(cond ((is-empty-bst? bst) bst)
      ((equal? x (get-entry bst)) (del-root bst))
      ((< x (get-entry bst)) (make-tree (get-entry bst) (deletion (get-leftsubtree bst) x) (get-rightsubtree bst)))
      (else (make-tree (get-entry bst) (get-leftsubtree bst) (deletion (get-rightsubtree bst) x))))
)

(define (del-root bst)
        (cond ((is-leaf? bst) empty)
              ((is-empty-bst? (get-rightsubtree bst)) (get-leftsubtree bst))
               ((is-empty-bst? (get-leftsubtree bst)) (get-rightsubtree bst))
               (else (make-tree (get-leftmost (get-rightsubtree bst)) (get-leftsubtree bst) (deletion (get-rightsubtree bst) (get-leftmost (get-rightsubtree bst)))))
))

(define (get-leftmost bst)
(cond ((is-empty-bst? bst) empty)
      ((is-empty-bst? (get-leftsubtree bst)) (get-entry bst))
(else (get-leftmost (get-leftsubtree bst)))))

(define (delete-leaves bt)
(cond ((null? bt) '())
((is-leaf? bt) '())
(else (make-tree (get-entry bt) (delete-leaves (get-leftsubtree bt)) (delete-leaves (get-rightsubtree bt))))
))

