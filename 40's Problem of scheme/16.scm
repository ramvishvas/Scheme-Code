; Problem 16(**):
;
; Drop every N'th element from a list
;
; Example:
;
; (drop '(a b c d e f g h i k) 3)
; ===> (a b d e g h k)

; When `cur-count` == 1, skip the current elements.
(define (drop lst k)
  (let rec ((lst lst) (cur-count k) (rslt '()))
    (cond
      ((null? lst) (reverse! rslt))
      ((<= cur-count 1)
       (rec (cdr lst) k rslt))
      (else
        (rec (cdr lst) (- cur-count 1) (cons (car lst) rslt))))))

; test.
;(drop '(a b c d e f g h i k) 3)
;(drop '() 3)
;(drop '(1 2 3 4 5 6) 0)
;(drop '(1 2 3 4 5 6) 1)
;(drop '(1 2 3 4 5 6) 2)

