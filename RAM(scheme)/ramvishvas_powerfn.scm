(define (pwrfn x n)
          (cond ((and (= 0 x) (= 0 n)) "meaning less")
                ((and (= 0 x) (not (= 0 n))) 0)
                ((= 0 n) 1)
                (else (* x (pwrfn x (- n 1))))))
