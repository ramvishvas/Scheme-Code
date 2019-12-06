; Problem 35(**):
;
; Determine the prime factors of a given positive integer.
;
; Construct a flat list containing the prime factors in ascending order.
;
; Example:
; (prime-factors 315)
; ==> (3 3 5 7)

(define (prime-factors num)
  (let loop ((num num) (divisor 2) (rslt '()))
    (cond
      ((> divisor num) (reverse! rslt))
      ((= (modulo num divisor) 0) 
       (loop (/ num divisor) divisor (cons divisor rslt)))
      (else
        (loop num (+ divisor 1) rslt)))))

; test
;(prime-factors 315)
;(prime-factors 111111111111)
;(prime-factors 2)
