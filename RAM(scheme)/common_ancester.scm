Question: Consider male members of a family with father-son relationship only, where at the maximum there are only two sons for a father. All the family members have unique names (you may assume that all have first names only). Person A is the ancestor of Person B if A is father / grand father / great grand father etc. of B. Person A is the descendent of person B if A is the son / grand son / great grand son etc. of B.Represent the family as a rooted binary tree; you must write all the necessary constructors and selectors.

1. Given a family and the name of a person, find the names of all his descendents.   
2. Given a family and the name of a person, find the names of all his descendents who have no sons.   
3. Given a family and the names of two persons, find their nearest common ancestor.

(define (make-family father left right)
(list entry left right))

(define (get-father family)
(car family))
(define (get-left family)
(cadr family))
(define (get-right family)
(caddr family))
(define (is-empty-family? family)
(null? family))
(define make-empty '())
(define (no-son? family)
(and (is-empty-family? (get-left family)) (is-empty-family? (get-right family))))

(define (alldescendants family name)
(cond ((is-empty-family? family) make-empty)
       ((equal? name (get-father family)) 
         (append (get-list (get-left family)) (get-list (get-right family))))
(else (append (alldescendants (get-right family) name) (alldescendants (get-left family) name)))
))

(define (get-list family)
(cond ((null? family) '())
      (else (append (get-list (get-left family)) (cons (get-entry family) (get-list (get-right family)))))))

(define (nosons family name)
(cond ((null? family) '())
((equal? (get-father family) name) 
(append (allleaves (get-left family)) (allleaves (get-right family))))
(else (append (nosons (get-left family) name) (nosons (get-right family) name)))))

(define (allleaves family)
(cond ((null? family) '())
((no-son? family) (cons (get-father family) '()))
(else (append  (allleaves (get-left family)) (allleaves (get-right family))))))

