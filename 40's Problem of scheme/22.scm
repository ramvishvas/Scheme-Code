; Problem 22(*)
;
; Create a list containing all integers within a given range.
;
; If first argument is smaller than second, produce a list in decreasing
; order.
;
; Example:
;
; (range 4 9)
; ===> (4 5 6 7 8 9)

(define (range low high)
  (let ((dec
          (if (<= low high) 
            (lambda (x) (- x 1))
            (lambda (x) (+ x 1)))))
    (let rec ((cur high) (rslt '()))
      (if (= cur low)
        (cons cur rslt)
        (rec (dec cur) (cons cur rslt))))))

; test
;(range 4 9)
;(range 9 4)
;(range 9 -4)
;(range -9 -4)
;(range 1 1)
