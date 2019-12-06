; Problem 31(**):
;
; Determine whether a given integer number is prime.
;
; Example:
; (is-prime 7)
; T

; We'll use traditional method(described below) here.
; Given a number N, find out (sqrt N), and check if any one of 
; (2, .. (sqrt N)) is a factor of N. If yes, then N is not a prime, otherwise
; it is a prime.

(define (is-prime? num)
  (let ((limit (sqrt num)))
    (let loop ((i 2))
      (cond 
        ((> i limit) #t)
        ((= (modulo num i) 0) #f)
        (else (loop (+ i 1)))))))

;(is-prime? 3)
;(map is-prime? '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))

