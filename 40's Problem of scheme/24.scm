; Problem 24(*):
;
; Lotto: Draw N different random numbers fromt he set 1..M
;
; The selected numbers shall be returned in a list
;
; Example:
;
; (lotto-select 6 49)
; ===> (23 1 17 33 21 37)
;
; HINT: Combine the solutions of problems P22 and P23.

; First, import results from P22 and P23.

; `range` from P22
(define (range low high)
  (let ((dec
          (if (<= low high) 
            (lambda (x) (- x 1))
            (lambda (x) (+ x 1)))))
    (let rec ((cur high) (rslt '()))
      (if (= cur low)
        (cons cur rslt)
        (rec (dec cur) (cons cur rslt)))))) 

; `random` from P23
(define (random max)
  (if (= max 0)
    0
    (modulo (sys-random) max)))

; `remove-at` from P20.
(define (remove-at lst k)
  (let rec ((lst lst) (count k) (rslt '()))
    (cond
      ((null? lst) (reverse! rslt))
      ((= count 1) 
       (rec (cdr lst) (- count 1) rslt))
      (else
        (rec (cdr lst) (- count 1) (cons (car lst) rslt))))))

; `rnd-select` from P23.
(define (rnd-select lst num)
  (let rec ((lst lst) 
            (len (length lst))
            (count num)
            (rslt '()))
    (cond
      ((null? lst) (reverse! rslt))
      ((<= count 0) (reverse! rslt))
      (else
        (let* ((rnd-index (random len))
               (elmt (list-ref lst rnd-index)))
          (rec (remove-at lst (+ rnd-index 1))
               (- len 1)
               (- count 1)
               (cons elmt rslt)))))))

; Use the result of P22 and P23.
(define (lotto-select num max)
  (rnd-select (range 1 max) num))

; test
;(lotto-select 6 49)

