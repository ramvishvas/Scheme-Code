; Problem 5:
;
; Reverse a list.


(define (my-reverse lst)
  (let rec ((lst lst) (result '()))
    (if (null? lst)
      result
      (rec (cdr lst) (cons (car lst) result)))))

; test
;(my-reverse '())
;(my-reverse '(a b c d e f g))
;(my-reverse '(a (b c) d e f g))
;(my-reverse '(a (b c) d e (f g)))

