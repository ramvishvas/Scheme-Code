; Problem2:
; Find the last but one box of a list.
; (my-but-last '(a b c d)) ===> (c d)

; First, assume lst always have more than 2 items including 2.
; thus (cddr lst) will always returns some value.
(define (my-but-last lst)
  (if (null? (cddr lst))
    lst
    (my-but-last (cdr lst))))


; Second 
; (my-but-last '())  ===> ()
; (my-but-last '(a)) ===> (a)
(define (my-but-last lst)
  (cond
    ((null? lst) lst)
    ((null? (cdr lst)) lst)
    ((null? (cddr lst)) lst)
    (else (my-but-last (cdr lst)))))

; test
(my-but-last '(a b c d))
(my-but-last '(a b c))
(my-but-last '(a b))
(my-but-last '(a))
