;this is also called flattning of the bst
;this method is basically converting bst to list
;inorder traversal this will always give sorted(incresing order) list
(define (make-bst val ls rs) (list val ls rs))
(define (get-bst-val bst) (car bst))
(define (get-bst-ls bst) (cadr bst))
(define (get-bst-rs bst) (caddr bst))
(define (preorder bst)
  (cond ((null? bst) '())
        (else  (append  (list (get-bst-val bst))
			(preorder (get-bst-ls bst))
                        (preorder (get-bst-rs bst))))))

