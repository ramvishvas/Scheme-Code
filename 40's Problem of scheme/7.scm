; Ploblem 7(**):
;
; Flatten a nested list structure.
;
; Transform a list, possibly holding lists as elements into a `flat` list by
; replacing each list with its elements(recursively).
;
; Example:
; (my-flatten '(a (b (c d) e))) ===> (a b c d e)

; Answer 1:
; First thought, with `append`
; this is slow and not tail recursive.
(define (my-flatten lst)
  (cond
    ((null? lst) lst)
    ((pair? (car lst)) 
     (append (my-flatten (car lst)) 
             (my-flatten (cdr lst))))
    (else (cons (car lst) (my-flatten (cdr lst))))))

; Answer 2:
; use tail recursion and `append`
(define (my-flatten lst)
  (define (rec lst rslt)
    (cond
      ((null? lst) rslt)
      ((pair? (car lst))
       (rec (cdr lst) 
            (append (rec (car lst) '()) rslt)))
      (else 
        (rec (cdr lst) (cons (car lst) rslt)))))
  (reverse! (rec lst '())))

; Answer 3:
; Introduce a global to improve performance (instead of using `append`)
; This however broke functional programming.
; Don't do this if the performance is not that critical.
(define (my-flatten lst)
  (let ((rslt '()))
    (let rec ((lst lst))
      (cond
        ((null? lst) #t) ; return values are not used
        ((pair? (car lst))
         (rec (car lst)) ; recursively flatten the sub-list.
         (rec (cdr lst)))
        (else
          (set! rslt (cons (car lst) rslt)) 
          (rec (cdr lst)))))
    (reverse! rslt)))
;test
;(my-flatten '(a (b (c d) e)))
;(my-flatten '((a b) c (d (e f) (g h)) i (j k (l m))))

