; Problem 10(*):
;
; Run-length encoding of a list
;
; Use the result of problem P09 to implement the so-called run-length encoding
; data compression method. Consecutive duplicates of elements are encoded as
; lists (N E) where N is the number of duplicates of the element E.
;
; Example:
; (encode '(a a a a b c c a a d e e e e))
; ===> ((4 a) (1 b) (2 c) (2 a) (1 d) (4 e))

; pack function from problem 09:
(define (pack lst)
  (let rec ((lst lst) (cur lst) (pkg '()) (rslt '()))
    (cond
      ((null? lst) (cdr (reverse! (cons pkg rslt))))
      ((equal? cur (car lst))
       (rec (cdr lst) cur (cons cur pkg) rslt))
      (else
        (rec (cdr lst)
             (car lst)
             (list (car lst))
             (cons pkg rslt))))))

; Use map to solve this problem
(define (encode lst)
  (map (lambda (lst) ; note that inner list will not be empty.
         (list (length lst) (car lst)))
    (pack lst)))

; test
;(encode '(a a a a b c c a a d e e e e))
;(encode '())
;(encode '(()))
;(encode '(() () 2 2 3 ))
