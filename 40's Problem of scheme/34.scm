; Problem 34(**):
;
; Calculate Euler's totient function phi(m).
;
; Euler's so-called totient function phi(m) is defined as the number of
; positive integers r (1 <= r < m) that coprime to m.
;
; Example: m = 10; r=1,3,7,9; thus phi(m)=4. Note the special case: 
; phi(1) = 1
;
; (totient-phi 10)
; ==> 4
;
; Find out what the value of phi(m) is if m is a prime number. Euler's totient
; function plays an important role in one of the most widely used public key
; cryptography methods(RSA). In this exercise you should use the most
; primitive method to calculate this function (there are smater ways that we
; shall discuss later).

; OK, so be primitive.
; 1. import the `coprime` function from P33.

; `gcd` from problem 32.
(define (gcd a b)
  (let loop ((a a) (b b))
    (if (= b 0)
      a
      (loop b (modulo a b)))))

(define (coprime a b)
  (= (gcd a b) 1))

; 2. test all numbers in range [1, m)
(define (totient-phi num)
  (let loop ((i 1) (total 0))
    (cond
      ((>= i num) total)
      ((coprime num i) (loop (+ i 1) (+ total 1)))
      (else (loop (+ i 1) total)))))

;(map totient-phi '(1 2 3 4 5 6 7 8 9 10))

