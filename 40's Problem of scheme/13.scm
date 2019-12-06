; Problem 13(**):
;
; Run-length encoding of a list(direct solution).
;
; Implement the so-called run-length encoding data compression method
; directly. I.e. don't explicitly create the sublists containing the
; duplicates, as in problem P09, but only count them. As in problem P11.
; Simplify the result list by replacing the singleton lists (1 X) by X.
;
; Example:
;
; (encode-direct '(a a a a b c c a a d e e e e))
; ===> ((4 a) b (2 c) (2 a) d (4 e))

; pack :-> used to generate output of an element according to its count.
; rec  :-> recursively encode elements.
;   it will add a junk element (0 lst) into `rslt` at first, so we need to cut
;   it off using `cdr` when return.
(define (encode-direct lst)
  (define (pack num elmt)
    (if (= num 1)
      elmt
      (list num elmt)))
  (let rec ((lst lst) (cur lst) (count 0) (rslt '()))
    (cond
      ((null? lst) (cdr (reverse! (cons (pack count cur) rslt))))
      ((equal? cur (car lst))
       (rec (cdr lst) cur (+ count 1) rslt))
      (else
        (rec (cdr lst) (car lst) 1 (cons (pack count cur) rslt))))))


; test
;(encode-direct '(a a a a b c c a a d e e e e))
;(encode-direct '())
;(encode-direct '(()))
;(encode-direct '(() ()))

