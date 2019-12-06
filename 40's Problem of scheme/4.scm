; Problem 4:
;
; Find the number of elements of a list


; use tail recursion
(define (number-of-elements lst)
  (let rec ((lst lst) (len 0))
    (if (null? lst) 
      len
      (rec (cdr lst) (+ len 1)))))

; test
;(number-of-elements '())
;(number-of-elements '(a))
;(number-of-elements '(a b))
;(number-of-elements '(a b c))
;(number-of-elements '(a b (c d)))
;(number-of-elements '(a b (c d) (e f) g))

