; Problem 25(*):
;
; Generate a random permutation of the elements of a list.
;
; Example:
; (rnd-permu '(a b c d e f))
; ===> (b a d c e f)
;
; HINT: use the solution of problem P23.

; Import solution of problem P 23.
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

; Use the result of P23.
(define (rnd-permu lst)
  (rnd-select lst (length lst)))


; test
;(rnd-permu '(a b c d e f))
