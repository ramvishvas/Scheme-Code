;1.Given a list of sublists find the sum of each sublist
;eg: ( (1 2 3) (4 3 1 5) (2 0) (3 4 5) (2 3 0 0 0))
;Output: (6 13 2 12 5)

(define (list_of_sum lst1)
        (cond ((null? lst1) '())
                  (else (cons (sum_of_list (car lst1)) (list_of_sum (cdr lst1))))))

(define (sum_of_list lst2)
      (cond ((null? lst2) 0)
                (else (+ (car lst2) (sum_of_list (cdr lst2))))))
