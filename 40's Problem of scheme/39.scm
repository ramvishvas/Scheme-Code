; Problem 39(*): 
;
; A list of prime numbers
;
; Given a range of integers by its lower and upper limit, construct a lit of
; all prime numbers in that range.

; We'll use sieve of eratosthenes to generate primes

(define (sieve upper)
  (let ((table (make-vector (+ upper 1) #t)))
    (define (next-prime index)
      (let loop ((i (+ index 1)))
        (cond
          ((> i upper) i)
          ((vector-ref table i) i)
          (else
            (loop (+ i 1))))))
    (define (collect)
      (let loop ((i 2) (rslt '()))
        (cond
          ((> i upper) (reverse! rslt))
          ((vector-ref table i) (loop (+ i 1) (cons i rslt)))
          (else
            (loop (+ i 1) rslt)))))
    (define (strike! prime)
      (let loop ((i (+ prime prime)))
        (if (> i upper)
          #t ; do nothing
          (begin
            (vector-set! table i #f)
            (loop (+ i prime))))))
    (let loop ((prime 2)) 
      (if (> prime upper)
        #t ; do nothing
        (begin
          (strike! prime) 
          (loop (next-prime prime)))))
    (collect)))

(define (prime-range low upper)
  (let loop ((primes (sieve upper)))
    (cond
      ((null? primes) '())
      ((>= (car primes) low) primes)
      (else (loop (cdr primes))))))

;(prime-range 11 30)
