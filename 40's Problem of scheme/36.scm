; Problem 36(**):
;
; Determine the prime factors of a given positive integer (2).
;
; Construct a list containing the prime factors and their multiplicity.
;
; Example:
; (prime-factors-mult 315)
; ===> ((3 2) (5 1) (7 1))

; Instead of directly generate the result, we first generate all prime factors
; using the result of P35 and then do a run-length encoding(i.e. group).
;
; Hint: The problem is similar to problem 13.

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

; test
;; Problem 36(**):
;
; Determine the prime factors of a given positive integer (2).
;
; Construct a list containing the prime factors and their multiplicity.
;
; Example:
; (prime-factors-mult 315)
; ===> ((3 2) (5 1) (7 1))

; Instead of directly generate the result, we first generate all prime factors
; using the result of P35 and then do a run-length encoding(i.e. group).
;
; Hint: The problem is similar to problem 13.

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

; test
;(prime-factors-mult 315)
;(prime-factors-mult 315)

