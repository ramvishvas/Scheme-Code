; Problem 33(*):
;
; Determine whether two positive integer numbers are coprime
;
; Two numbers are coprime if their greatest common divisor equals 1.
;
; Example:
; (coprime 35 64)
; ==> #t

; `gcd` from problem 32.
(define (gcd a b)
  (let loop ((a a) (b b))
    (if (= b 0)
      a
      (loop b (modulo a b)))))

(define (coprime a b)
  (= (gcd a b) 1))

; test
;(coprime 35 64)
;(coprime 35 63)

