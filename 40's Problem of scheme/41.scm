; Problem 41(**):
;
; A list of Goldbach compositions.
;
; Given a range of integers by its lower and upper limit, print a list of all
; even numbers and their Goldbach composition.
;
; Example:
; (goldbach-list 9 20)
; 10 = 3 + 7
; 12 = 5 + 7
; 14 = 3 + 11
; 16 = 3 + 13
; 18 = 5 + 13
; 20 = 3 + 17
;
; In most cases, if an even number is written as the sum of two prime numbers,
; one of them is very small, Very rarely, the primes are both bigger than say
; 50. Try to find out how many cases there are in the range 2..3000.
;
; Example (for a print limit of 50):
;
; (goldbach-list 1 2000 50)
; 992 = 73 + 919
; 1382 = 61 + 1321
; 1856 = 67 + 1789
; 1928 = 61 + 1867

; find next prime in table(vector).
(define (next-prime table index upper)
      (let loop ((i (+ index 1)))
        (cond
          ((> i upper) i)
          ((vector-ref table i) i)
          (else
            (loop (+ i 1))))))

(define (sieve upper)
  (let ((table (make-vector (+ upper 1) #t)))
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
          (loop (next-prime table prime upper)))))
    table))

; generate even numbers and ites goldbach pairs.
; (goldbach-list 9 20)
; ===>
; ((10 (3 7))
; (12 (5 7))
; (14 (3 11))
; (16 (3 13))
; (18 (5 13))
; (20 (3 17)))
(define (goldbach-list low upper)
  (if (odd? low) (goldbach-list (+ low 1) upper)
    (let ((primes (sieve upper)))
      (define (goldbach num)
        (let loop ((prime1 2))
          (let ((prime2 (- num prime1)))
            (cond
              ((< prime2 prime1) '())
              ((vector-ref primes prime2) (list prime1 prime2))
              (else
                (loop (next-prime primes prime1 upper)))))))
      (let loop ((i low) (rslt '()))
        (cond
          ((> i upper) (reverse! rslt))
          (else
            (loop (+ i 2) (cons (list i (goldbach i))
                                rslt))))))))


(use srfi-1) ; import `filter` `dolist`

; filter out those pairs 
(define (goldbach-list-limit low upper limit)
  (filter (lambda (x)
            (and (>= (caadr x) limit)
                 (>= (cadadr x) limit)))
          (goldbach-list (if (> low 2) low 4) upper)))
; test
(goldbach-list 9 20)
(goldbach-list-limit 1 2000 50)

; print a goldbach list
(define (print-goldbach lst)
  (dolist (x lst)
    (print (car x) " = " (caadr x) " + " (cadadr x))))

(print-goldbach (goldbach-list 9 20))
(print-goldbach (goldbach-list-limit 1 2000 50))S
