; Problem 32(**):
;
; Determine the greatest common divisor of two positive integer numbers.
;
; Use Euclid's algorithm.
; Example:
; (gcd 36 63)
; ===> 9

(define (gcd a b)
  (let loop ((a a) (b b))
    (if (= b 0)
      a
      (loop b (modulo a b)))))


; test
;(gcd 36 63)
;(gcd 63 36)
;(gcd 16 24)
;(gcd 24 16)

