; Problem 19(**):
;
; Rotate a list N places to the left
;
; Examples:
;
; (rotate '(a b c d e f g h) 3)
; ===> (d e f g h a b c)
; 
; (rotate '(a b c d e f g h) -2)
; ===> (g h a b c d e f)
; 
; Hint: use the predefined functions length and append, as well as the result
; of problem 17.

; Answer 1:
; Note that learn to use the result of problems we've solved is important.

; split function from P17.
(define (split lst k)
  (let rec ((lst lst) (first '()) (count k))
    (cond
      ((null? lst) (list (reverse! first) '()))
      ((<= count 0) (list (reverse! first) lst))
      (else
        (rec (cdr lst) (cons (car lst) first) (- count 1))))))

(define (rotate lst shift)
  (let ((len (length lst)))
    (if (= len 0) ; situation when lst is '()
      lst
      (let* ((len (length lst)) 
             (shift (modulo shift len)) 
             (splited (split lst shift))) 
        (append (cadr splited) (car splited))))))

; test
;(rotate '(a b c d e f g h) 3)
;(rotate '(a b c d e f g h) -2)
;(rotate '() 10)
;(rotate '(a b c d e f g h) 0)
;(rotate '(a b c d e f g h) 8)

