;Binary Tree: Family Tree : Descendent Tree
;Question: Consider male members of a family with father-son relationship only, where at the maximum there are only two sons for a father. All the family members have unique names (you may assume that all have first names only). Person A is the ancestor of Person B if A is father / grand father / great grand father etc. of B. Person A is the descendent of person B if A is the son / grand son / great grand son etc. of B.Represent the family as a rooted binary tree; you must write all the necessary constructors and selectors.

;1. Given a family and the name of a person, find the names of all his descendents.   
;2. Given a family and the name of a person, find the names of all his descendents who have no sons.   
;3. Given a family and the names of two persons, find their nearest common ancestor.  

(define (make-family node lb rb)     ;constructor, i/p-node,left-branch,right-branch o/p-family-tree
             (list node lb rb))
   
(define (get-node family)                  ;selector, i/p-family o/p-node/root
             (car family))
   
(define (get-lb family)                       ;selector, i/p-family o/p-left-branch
        (cadr family))
   
(define (get-rb family)                      ;selector ,i/p-family o/p-right branch
             (caddr family))
   
(define (mef)                                      ;makes a empty family
             '())
   
(define (empty? family)                     ;i/p-family o/p-#t for empty else #f
             (null? family))
     
(define (first a)                                    ;returns 1st of list 
             (car a))
   
(define (rest a)                                    ;returns list except first element
             (cdr a))
    
(define (main1 name family)             ;returns all the descendants of person with name "name" in family 
              (cond ((empty? family) '())
                     ((equal? name (get-node family)) 
		      (append (get-list (get-lb family))
                              (get-list (get-rb family))))
                      (else (append (main1 name (get-lb family))
                                    (main1 name (get-rb family))))))     
                           
(define (get-list family)                      ;returns list of members in the family
             (cond ((empty? family) '())
                   (else (append (get-list (get-lb family))
                                  (list (get-node family))
                                  (get-list (get-rb family))))))
                       
(define (main2 name family)              ;returns all descendants with no sons in the family of person with name "name"
             (cond ((empty? family) '())
                   ((equal? name (get-node family)) 
		    (append (get-list2 (get-lb family))
                            (get-list2 (get-rb family))))
                   (else (append (main2 name (get-lb family)) 
				 (main2 name (get-rb family))))))
         
    
(define (get-list2 family)                   ;returns all the descendants of family with no sons
             (cond ((empty? family) '())
                    ((and (empty? (get-lb family)) (empty? (get-rb family)))
                     (list (get-node family)))
                    (else (append (get-list2 (get-lb family)) 
				  (get-list2 (get-rb family))))))                           
             
(define (member-fam? name family) ;returns #t if person with "name" exists in the family
		(cond ((equal? (get-node family) name) #t)
		      (else (mem? name (main1 (get-node family) family)))))
             
 
(define (mem? name list-fam)           ;returns #t if person with "name" exists in the family
            (cond ((empty? list-fam) #f)
                   ((eq? name (first list-fam)) #t)
                   (else (mem? name (rest list-fam)))))
             
(define (main3 n1 n2 family)               ;returns common ancestor of person1 and person2 with names n1 and n2
            (cond ((and (member-fam? n1 family) (member-fam? n2 family))
                   (cond ((and (member-fam? n1 (get-lb family)) (member-fam? n2 (get-lb family)))
                           (main3 n1 n2 (get-lb family)))
                         ((and (member-fam? n1 (get-rb family)) (member-fam? n2 (get-rb family)))
                          (main3 n1 n2 (get-rb family)))
                         (else (get-node family))))
               	 (else 'invalid-names)))      
              
;sample examples:
            
;(define a (make-family 'ab (make-family 'cd (mef) (mef)) (make-family 'ef (mef) (mef))))

;(define b (make-family 'gh (make-family 'ij (mef) (mef)) (make-family 'kl (mef) (mef))))

;(define c (make-family 'mn a b))

;(main1 'ab c) => o/p => (cd ef)

;(main2 'mn c) => o/p => (cd ef ij kl)

;(main3 'cd 'ef c) => o/p => ab
