;Program 3: Find the sum of the series : x - (x^3)/3! + (x^5)/5! +......+ (x^n)/n! where x and n are the inputs from user.

(define (sin_expn x n)
        (cond ((and (= x 0) (= n 0) "meaning less"))
                 ((= n 0)"meaning less")
                 ((= n 1) x)
                 (else (+ (/ (pwrfn x (- (* 2 n) 1) (fact (- (* 2 n) 1)) (sin_expn x (- n 1))))))
                 
(define (pwrfn x n)
          (cond ((and (= 0 x) (= 0 n)) "meaning less")
                ((= 0 x) 0)
                ((= 0 n) 1)
                (else (* x (pwrfn x (- n 1))))))

(define (fact n)
          (cond ((or (= 0 n) (= 1 n)) 1)
                (else (* n (fact (- n 1))))))
