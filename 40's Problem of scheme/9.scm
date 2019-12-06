; Problem 09(**):
;
; Pack consecutive duplicates of list elements into sublists.
;
; If a list contains repeated elements they should be placed in seperate
; sublists.
;
; Example:
; (pack '(a a a a b c c a a d e e e e))
; ===> ((a a a a ) (b) (c c) (a a) (d) (e e e e))

; lst: remaining list
; cur: current repeating element
; pkg: current packing list.
; rslt: result.
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
(pack '(a a a a b c c a a d e e e e) )
(pack '()) ; ===> ()
(pack '(())) ; ===> ((()))
(pack '(() () 2 2 3 )) ; ===> ((() ()) (2 2) (3))

