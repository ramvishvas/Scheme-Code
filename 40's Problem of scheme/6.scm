; Problem 6:
; 
; Find out whether a list is a palindrome.
; A palindrome can be read forward or backward; e.g. (x a m a x)


; Answer 1:
; Use build in functions
(define (palindrome? lst)
  (equal? lst (reverse lst)))

; Answer 2:
; idea borrowed from (http://www.informatimago.com/develop/lisp/l99/p06.lisp)
;
; Construct a list of half size in reverse order and compare them.
; Walk a list with twice speed to count the *half size*
; (a b c d e f) => 
;   second-half: (d e f)
;   rev-first-half: (c b a)
(define (palindrome? lst)
  (let rec ((second-half lst) (rev-first-half '()) (count lst))
    (cond
      ((null? count) 
       (equal? second-half rev-first-half))
      ((null? (cdr count))
       (equal? (cdr second-half) rev-first-half))
      (else
        (rec (cdr second-half) 
             (cons (car second-half) rev-first-half)
             (cddr count))))))

; test 
;(palindrome? '(x a m a x))
;(palindrome? '(x a m a))
;(palindrome? '((x a) m (a x)))
;(palindrome? '((x a) m (x a)))
;(palindrome? '((x a) m x a))

