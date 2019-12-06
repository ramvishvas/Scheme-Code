; Problem 3:
; Find the K'th element of a list
; The first element in the list is number 1
; 
; Example:
; (element-at '(a b c d e) 3) ===> c

; handle error input simply.
; return '() if K is out of range
; note that error range also linear time instead of constant time.
(define (element-at lst k)
  (cond
    ((null? lst) '())
    ((= k 1) (car lst))
    (else (element-at (cdr lst) (- k 1)))))

;(element-at '(a b c d e) 1)
;(element-at '(a b c d e) 3)
;(element-at '(a b c d e) 8)
;(element-at '(a b c d e) -1)
