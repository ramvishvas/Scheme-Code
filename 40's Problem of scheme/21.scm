; Problem 21(*):
;
; Insert an element at a given position into a list.
;
; Example:
;
; (insert-at 'alfa '(a b c d) 2)
; ===> (a alfa b c d)

; Answer 1:
; use the result of P17.

; Function split from P17.
; (split '(a b c d e f g h i k) 3)
; ===> ((a b c) (d e f g h i k))
(define (split lst k)
  (let rec ((lst lst) (first '()) (count k))
    (cond
      ((null? lst) (list (reverse! first) '()))
      ((<= count 0) (list (reverse! first) lst))
      (else
        (rec (cdr lst) (cons (car lst) first) (- count 1))))))

; Again, taking advantage of the result we've got will reduce the difficulty
; of solving problems.
(define (insert-at elmt lst pos)
  (let* ((splited (split lst pos))
         (head (car splited))
         (last (cadr splited)))
    (append head (cons elmt last))))


; Answer 2:
; direct solution. O(n) for build the list, another O(n) for reverse the
; result.
(define (insert-at elmt lst pos)
  (if (<= pos 1) (cons elmt lst)
    (let rec ((lst lst) (count pos) (rslt '()))
      (cond
        ((= count 1)    ; this should come frst. otherwise (ins 'a '(1 2) 3)
                        ; will fail
         (append (reverse! rslt) (cons elmt lst)))
        ((null? lst) 
         (if (> count 1)
           (reverse! (cons elmt rslt))
           (reverse! rslt)))
        (else
          (rec (cdr lst) (- count 1) (cons (car lst) rslt)))))))


; test
;(insert-at 'alfa '(a b c d) 2)
;(insert-at 'a '() 1)
;(insert-at 'a '() 20)
;(insert-at 'a '(1 2 3) 4)
;(insert-at 'a '(1 2 3) -1)
;(insert-at 'a '(1 2 3) 12)

