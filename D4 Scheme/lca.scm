;this the code to calculate the lowest common ancester between two given nodes
;it is gaurenteed that two nodes for which we want to calculate lca are present in the bst
;the lca of node with values a and b is defined as the lowest node form the root which is
;parent of both a and b or which has both a and b as its decendents
;using bst property we can simply identify this.travel from root recursively and find first node
;whose value is either between a and b or same as a or b then this node will be the lca of a and b
;we allow a node to be desendent of itself
(define (make-bst val ls rs) (list val ls rs))
(define (get-bst-val bst) (car bst))
(define (get-bst-ls bst) (cadr bst))
(define (get-bst-rs bst) (caddr bst))
(define (lca bst a b)
   (cond ((null? bst) -1)
	 ((and (< a (get-bst-val bst)) (< b (get-bst-val bst)))
          (lca (get-bst-ls bst) a b))
	 ((and (> a (get-bst-val bst)) (> b (get-bst-val bst)))
          (lca (get-bst-rs bst) a b))
	(else (get-bst-val bst))))
	       

	 

