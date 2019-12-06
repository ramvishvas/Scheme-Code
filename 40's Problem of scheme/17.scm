; Problem 17(*):
;
; Split a list into two parts; the length of the first part is given.
;
; Do not use any predefined predicates.
;
; Example:
;
; (split '(a b c d e f g h i k) 3)
; ===> ((a b c) (d e f g h i k))

; first part of the split is stored in `first`
(define (split lst k)
  (let rec ((lst lst) (first '()) (count k))
    (cond
      ((null? lst) (list (reverse! first) '()))
      ((<= count 0) (list (reverse! first) lst))
      (else
        (rec (cdr lst) (cons (car lst) first) (- count 1))))))

; test
;(split '(a b c d e f g h i k) 3)
;(split '() 3)
;(split '(2 3 4) -1)

