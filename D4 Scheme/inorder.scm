;this is also called flattning of the bst
;this method is basically converting bst to list
;inorder traversal this will always give sorted(incresing order) list
(define (make-bst val ls rs) (list val ls rs))
(define (get-bst-val bst) (car bst))
(define (get-bst-ls bst) (cadr bst))
(define (get-bst-rs bst) (caddr bst))
(define (inorder bst)
  (cond ((null? bst) '())
        (else (append (inorder (get-bst-ls bst))
                      (list (get-bst-val bst))
                      (inorder (get-bst-rs bst))))))

