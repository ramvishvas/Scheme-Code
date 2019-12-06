; Problem 14(*):
;
; Duplicate the elements of a list
;
; Example:
; (dupli '(a b c c d))
; ===> (a a b b c c c c d d)

; Direct solution, faster.
(define (dupli lst)
  (let rec ((lst lst) (rslt '()))
    (if (null? lst)
      (reverse! rslt)
      (rec (cdr lst) (cons (car lst) (cons (car lst) rslt))))))

; below is a implmentation of `append-map`
; (use srfi-1) ; in Gauche to load `append-map` function
(define (append-map proc lst)
  (let rec ((lst lst) (rslt '()))
    (if (null? lst) 
      (reverse! rslt)
      (rec (cdr lst) (append (proc (car lst)) rslt)))))

; use high-order functions
(define (dupli lst)
  (append-map (lambda (x) (list x x)) lst))


;(dupli '(a b c c d))
;(dupli '(() ()))
