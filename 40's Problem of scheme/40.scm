; Problem 40:
;
; Goldbach's conjecture.
;
; Goldbach's conjecture says that every positive even number greater than 2 is
; the sum of two prime numbers. Example: 28 = 5+ 23. It is one of the most
; famous facts in number theory that has not been proved to be correct in the
; general case. It has been numerically confirmed up to very large numbers(
; much larger than we can go with our Prolog system). Write a predicate to
; find the two prime numbers that sum up to a given even integer.
;
; Example:
;
; (goldbach 28)
; ===> (5 23)

; First, we import the result of P39, which contains functions to generate a
; list prime numbers.
; We'll modify it a little: return a vector instead of list and use it as a
; hash table.

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
    table))

(define (goldbach number)
  (if (odd? number) '()
    (let ((primes (sieve number))) 
      (define (next-prime index) 
        (let loop ((i (+ index 1))) 
          (cond 
            ((> i number) i) 
            ((vector-ref primes i) i) 
            (else 
              (loop (+ i 1))))))
      (let loop ((prime1 2))
        (let ((prime2 (- number prime1)))
          (cond
            ((< prime2 prime1) '())
            ((vector-ref primes prime2) (list prime1 prime2))
            (else
              (loop (next-prime prime1)))))))))

;(time (goldbach 288888888))
