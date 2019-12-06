; Problem 15(**):
;
; Replicate the elements of a list a given number of times.
;
; Example:
; (repli '(a b c) 3)
; ===> (a a a b b b c c c)

; Answer 1:
; Use high-order function `append-map`

; Below is a sample implementation.
; `append-map` is a SRFI-1 function and is usually embedded in scheme
; implementations.
; (use srfi-1) ; in Gauche to load `append-map` function
(define (append-map proc lst)
  (let rec ((lst lst) (rslt '()))
    (if (null? lst) 
      (reverse! rslt)
      (rec (cdr lst) (append (proc (car lst)) rslt)))))

; (genlist 'a 3) ===> (a a a)
(define (repli lst repeat)
  (define (genlist elmt repeat)
    (let rec ((elmt elmt) (count repeat) (rslt '()))
      (if (<= count 0)
        rslt
        (rec elmt (- count 1) (cons elmt rslt)))))
  (append-map (lambda (elmt) (genlist elmt repeat)) lst))

; Answer 2
; Direct solution.
; when `cur-count` == 0, move to the next element.
(define (repli lst repeat)
  (let rec ((lst lst) (cur-count repeat) (rslt '()))
    (cond
      ((null? lst) (reverse! rslt))
      ((<= cur-count 0)
       (rec (cdr lst) repeat rslt))
      (else
        (rec lst (- cur-count 1) (cons (car lst) rslt))))))

; test
(repli '(a b c) 0)
(repli '(a b c) 3)
(repli '(a b c) 10)
(repli '() 10)
(repli '(()) 0)
