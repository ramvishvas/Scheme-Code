; Problem 08(**):
;
; Eliminate consecutive duplicates of list elements.
;
; If a list contains repeated elements they should be replaced with a single
; copy of the element. the order of the elements should not be changed.
;
; Example:
; (compress '(a a a a b c c a a d e e e e)) ===> (a b c a d e)

; Answer 1:
(define (compress lst)
  (let rec ((lst lst) (cur lst) (rslt '())) ; initialize cur to lst to avoid equivalence 
    (cond
      ((null? lst) (reverse! rslt))
      ((equal? cur (car lst))
       (rec (cdr lst) cur rslt))
      (else
        (rec (cdr lst) (car lst) (cons (car lst) rslt))))))

;(compress '(a a a a b c c a a d e e e e))
;(compress '((a) (a) (a) (b) (c) (c) c a a (a) (a)))
;(compress '(() () ()))

