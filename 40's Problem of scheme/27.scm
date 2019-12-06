; Problem 27(**):
;
; Group the elements of a set into disjoint subsets.
;
; a) In how many ways can a group of 9 people work in 3 disjoint subgroups of
; 2, 3 and 4 persons? Write a function that generates all the possibilities
; and returns them in a list.

; Example:
; (group3 '(aldo beat carla david evi flip gary hugo ida))
; (((ALDO BEAT) (CARLA DAVID EVI) (FLIP GARY HUGO IDA)) ... )

; b)  Generalize the above predicate in a way that we can specify a list of
; group sizes and the predicate will return a list of groups.

; Example:
; (group '(aldo beat carla david evi flip gary hugo ida) '(2 2 5))
; (((ALDO BEAT) (CARLA DAVID) (EVI FLIP GARY HUGO IDA)) ... )

; Note that we do not want permutations of the group members; i.e. ((ALDO
; BEAT) ...) is the same solution as ((BEAT ALDO) ...). However, we make a
; difference between ((ALDO BEAT) (CARLA DAVID) ...) and ((CARLA DAVID) (ALDO
; BEAT) ...).

; You may find more about this combinatorial problem in a good book on
; discrete mathematics under the term "multinomial coefficients". 


; for gauche: import `lset-difference` which is defined in SRFI-1
(use srfi-1)
; test how `lset-difference` works
(lset-difference equal? '(1 2 3) '(3 4))
(define (lset-diff list1 list2)
  (lset-difference equal? list1 list2))

; `combination` from P26
(define (combination num lst)
  (let ((len (length lst)))
    (cond
      ((< len num) '())
      ((= num 1) (map (lambda (x) (list x)) lst))
      (else
        (let loop ((lst lst) (count num) (rslt '()))
          (if (or (null? lst) (< len count))
            rslt
            (loop (cdr lst) (+ count 1)
                  (append
                    rslt
                    (map (lambda (x)
                           (cons (car lst) x))
                         (combination (- num 1) (cdr lst)))))))))))

(define (group lst parts)
  (if (null? parts) '(())
    (let loop ((combs (combination (car parts) lst)) 
               (rslt '()))
      (if (null? combs) rslt
        (loop (cdr combs)
              (append
                rslt
                (map (lambda (x) (cons (car combs) x))
                     (group (lset-diff lst (car combs))
                            (cdr parts)))))))))

; test
;(length (group '(1 2 3 4 5 6 7 8 9) '(2 3 4)))
;(group '(1 2 3) '(1 1))
;(map length
 ;    (list 
  ;     (group '(1 2 3 4 5 6 7 8 9) '(2 3 4))
   ;    (group '(1 2 3 4 5 6 7 8 9) '(2 2 5))))
