; Problem 38(*): 
;
; Compare the two methods of calculating Euler's totient function.
;
; Use the solutions of problems P34 and P37 to compare the algorithms. Take
; the number of logical inferences as a measure for efficiency. Try to
; calculate phi(10090) as an example.

; First, import solutions from P34
;=====================================================================
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

; Second, import solutions from P37
;=====================================================================
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

;(time (totient-phi 10090))
;(time (phi 10090))

