; Problem 11(*):
;
; Modified run-length encoding.
;
; Modify the result of problem P10 in such a way that if an element has no
; duplicates it is simply copied into the result list. Only elements with
; duplicates are transferred as (N E) lists.
;
; Example:
; (encode-modified '(a a a a b c c a a d e e e e))
; ===> ((4 a) b (2 c) (2 a) d (4 e))

; `pack` function from problem 09.
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

; `encode` function from problem 10. 
(define (encode lst)
  (map (lambda (lst) ; note that inner list will not be empty.
         (list (length lst) (car lst)))
    (pack lst)))

; Answer 1: 
; extend the result of Problem 09.
(define (encode-modified lst)
  (map (lambda (lst) ; note that inner list will not be empty.
         (if (null? (cdr lst))
           (car lst)
           (list (length lst) (car lst))))
    (pack lst)))

; Answer 2:
; Modify the result of Problem 10.
(define (encode-modified lst)
  (map (lambda (lst)
         (if (= (car lst) 1)
           (cadr lst)
           lst))
       (encode lst)))
; test
;(encode-modified '(a a a a b c c a a d e e e e))
;(encode-modified '())
;(encode-modified '(()))
;(encode-modified '(() () 2 2 () 3 ))
