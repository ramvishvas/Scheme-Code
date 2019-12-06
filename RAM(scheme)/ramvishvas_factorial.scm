(define (factn n)
          (cond ((or (= 0 n) (= 1 n)) 1)
                (else (* n (factn (- n 1))))))
