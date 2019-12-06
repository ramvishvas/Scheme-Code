; Problem 23(**):
;
; Extract a given number of randomly selected elements from a lista.
;
; The selected item shall be returned in a list.
;
; Example:
;
; (rnd-select '(a b c d e f g h) 3)
; ===> (e d a)
;
; Hint: Use the built-in random number generator and the result of problem
; P20.


; Note that `random` generator is not built in for scheme, however almost all
; implementations provide their own. It's also a feature described in SRFI-27
; We'll use gauche's random generator here.
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


; An intuitive solution:
; 1. find a random index
; 2. fetch the corresponding element
; 3. remove this element from the original list
; 4. repeat 1 until there's enough elments.

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

; test
;(rnd-select '(a b c d e f g h) 3)

; As we see here that functional programming will cost more when requiring
; small modification to data structure, cause it will actually generate a new
; one for us. Imperative languages will do faster.
;
; However, Functional programming will increase the stablity of programs. And
; APIs more robust. 
;
; A blade have two sides.

