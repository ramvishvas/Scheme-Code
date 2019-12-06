(define (reverse list)
(cond ((null? list) '())      
      (else (cons (reverse (cdr list)) (car list) )
)
)
)

