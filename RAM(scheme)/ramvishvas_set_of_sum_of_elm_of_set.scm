(define (es ls1)
          (cond ((null? ls1) '( ))
                (else (cons (sm (car ls1)) (es (cdr ls1))))))
(define (sm n)
          (cond ((= n 0) 0)
                (else (/ (* n (+ n 1)) 2))))
