; Problem 18:
;
; Extract a slice from a list
;
; Given two indices, I and K, the slice is the list containing the elements
; between the I'th and K'th element of the original list (both limits
; included). Start counting the elements with 1.
;
; Example:
;
; (slice '(a b c d e f g h i k) 3 7)
; ===> (c d e f g)


(define (slice lst low high)
  (let rec ((lst lst) (cur 1) (rslt '()))
    (cond
      ((null? lst) (reverse! rslt))
      ((and (>= cur low) (<= cur high))
       (rec (cdr lst) (+ cur 1) (cons (car lst) rslt)))
      ((< cur low)
       (rec (cdr lst) (+ cur 1) rslt))
      (else
        (reverse! rslt)))))


; test
;(slice '(a b c d e f g h i k) 3 7)
;(slice '() 3 8)
;(slice '(1 2 3 4 5 6) 8 3)
;(slice '(1 2 3 4 5 6) 3 3)
;(slice '(1 2 3 4 5 6) 3 12)
;(slice '(1 2 3 4 5 6) -1 5)
