; Problem 37(**):
;
; Calculate Euler's totient function phi(m) (improved).
;
; See problem P34 for the definition of Euler's toient function. If the list
; of the prime factors of a number m is known in the form of problem P36 then
; the function phi(m) can be efficiently calculated as follows: Let ((p1 m1)
; (p2 m2) (p3 m3) ...) be the list of prime factors (and their multiplicities)
; of a given number m. Then phi(m) can be calculated with the following
; formula:
;
; phi(m) = (p1-1)*p1**(m1-1) + (p2-1)*p2**(m2-1) + (p3-1)*p3**(m3-1) + ...
;
; Note that a**b stands for the b'th power of a.

; Comment: the formula in the problem description is wrong, the correct one
; is as follows:
; phi(m) = ((p1-1)*p1**(m1-1)) * ((p2-1)*p2**(m2-1)) * ((p3-1)*p3**(m3-1)) *
; ...

; First, import results of P36.

; `prime-factors` from P35.
(define (prime-factors num)
  (let loop ((num num) (divisor 2) (rslt '()))
    (cond
      ((> divisor num) (reverse! rslt))
      ((= (modulo num divisor) 0) 
       (loop (/ num divisor) divisor (cons divisor rslt)))
      (else
        (loop num (+ divisor 1) rslt)))))

; similar to P13
; (encode '(a a a a b c c a a d e e e e)) 
; ===> ((a 4) (b 1) (c 2) (a 2) (d 1) (e 4))
(define (encode lst)
  (let loop ((lst lst) (cur lst) (count 0) (rslt '()))
    (cond
      ((null? lst) (cdr (reverse! 
                          (cons (list cur count) rslt)))) 
      ((equal? (car lst) cur)
       (loop (cdr lst) cur (+ count 1) rslt))
      (else
        (loop (cdr lst) 
              (car lst) 
              1 
              (cons (list cur count) rslt))))))

(define (prime-factors-mult num)
  (encode (prime-factors num)))

(define (phi m)
  (apply * (map (lambda (pair)
                  (let ((prime (car pair))
                        (multi (cadr pair)))
                    (* (- prime 1) (expt prime (- multi 1)))))
                (prime-factors-mult m))))

; test
;(phi 10)
;(map phi '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
;(prime-factors-mult 6)
;(phi 6)

