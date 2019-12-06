(define (my-last lst)
  (cond
    ((null? lst) '())
    ((null? (cdr lst)) (car lst))
    (else (my-last (cdr lst)))))

; test
;(my-last '(a b c d))
;(my-last '())
;(my-last '(1 2))

