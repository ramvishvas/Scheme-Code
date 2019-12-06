; Problem 20:
;
; Remove the K'th element from a list.
;
; Example:
;
; (remove-at '(a b c d) 2)
; ===> (a c d)

; Because the representation of scheme's list, we have to recreate a list from
; scratch
(define (remove-at lst k)
  (let rec ((lst lst) (count k) (rslt '()))
    (cond
      ((null? lst) (reverse! rslt))
      ((= count 1) 
       (rec (cdr lst) (- count 1) rslt))
      (else
        (rec (cdr lst) (- count 1) (cons (car lst) rslt))))))

; Answer 2:
; With side effect: modify elements in place.
; note that it will change the input.
; Do NOT use this if you have enough reasons.
(define (remove-at! lst k)
  (let rec ((inner-lst lst) (count k) (prev #f))
    (cond
      ((null? inner-lst) lst)
      ((= count 1)
       (if prev 
         (begin
           (set-cdr! prev (cdr inner-lst)) ; side effect
           lst)      ; stop and return value
         (cdr lst))) ; the element to be cut is the first one 
      (else
        (rec (cdr inner-lst) (- count 1) inner-lst)))))


; test
;(remove-at '(a b c d) 2)
;(remove-at '(a b c d) 0)
;(remove-at '(a b c d) 8)
;(remove-at '(a b c d) 4)
;(remove-at! '(a b c d) 2)
;(remove-at! '(a b c d) 0)
;(remove-at! '(a b c d) 8)
;(remove-at! '(a b c d) 4)
;(define *x* '(a b c d))  
;(define *y* (remove-at! *x* 2)) ; both *x* and *y* were (a b d)
