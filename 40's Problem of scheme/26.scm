; Problem 26:
;
; Generate the combinitions of K distinct objects chosen from the N elements
; of a list
;
; In how many ways can a committee of 3 be chosen from a group of 12 people?
; We all know that there are C(12, 3) = 220 possibilities (C(N,K) denotes the
; well-known binomial coefficients). For pure mathematicians, this result may
; be great. But we want to really generate all the possibilities in a list.
;
; Example:
;
; (combination 3 '(a b c d e f))
; ===> ((a b c) (a b d) (a b e) ...)


; implementation idea:
; loop + recursion.
; '(a b c d) , generate its combinitions of 2 items.
; 1. take out a, generate combinitions recursively for '(b c d) with 1 item
;    it is '((b) (c) (d))
; 2. add item 'a to the top of each item that returned, and we got
;    '((a b) (a c) (a d))
; 3. take out item 'b and do the same, we got '((b c) (b d))
; 4. combine the lists together => '((a b) (a c) (a d) (b c) (b d))
; 5. repeat step 1.
(define (combination n lst)
  (let ((len (length lst)))
    (cond
      ((= len n) (list lst))
      ((= 1 n) (map (lambda (x) (list x)) lst))
      (else
        (let loop ((lst lst) (count n) (rslt '()))
          (if (or (null? lst) (> count len)) 
            rslt
            (loop (cdr lst)
                  (+ count 1)
                  (append rslt
                          (map (lambda (x) (cons (car lst) x))
                               (combination (- n 1) (cdr lst)))))))))))


;(combination -1 '(a b c d e f))
;(combination 0 '(a b c d e f))
;(combination 1 '(a b c d e f))
;(combination 2 '(a b c d e f))
;(combination 3 '(a b c d e f))
;(combination 4 '(a b c d e f))
;(combination 5 '(a b c d e f))
;(combination 6 '(a b c d e f))
;(combination 7 '(a b c d e f))
;(combination 5 '(a b c d e f))
;(combination 2 '(a b c d e f g h i j k))
