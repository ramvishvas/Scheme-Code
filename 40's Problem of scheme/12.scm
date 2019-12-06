; Problem 12(**):
; 
; Decode a run-length encoded list.
;
; Given a run-length code list generated as specified in problem P11.
; Construct its uncompressed version.
;
; (decode '((4 a) b (2 c) (2 a) d (4 e)))
; ===> (a a a a b c c a a d e e e e)



; decode-element is used to generate lists for pairs like '(4 a)
; (decode-element '(4 a)) ===> (a a a a)
; A performance consideration is the usage of `append`.
(define (decode lst)
  (define (decode-element elmt)
    (let rec ((times (car elmt)) (elmt (cadr elmt)) (rslt '()))
      (if (= 0 times)
        rslt
        (rec (- times 1) elmt (cons elmt rslt)))))
  (let rec ((lst lst) (rslt '()))
    (cond
      ((null? lst) (reverse! rslt))
      ((pair? (car lst))
       (rec (cdr lst) (append (decode-element (car lst)) rslt)))
      (else
        (rec (cdr lst) (cons (car lst) rslt))))))

;(decode '((4 a) b (2 c) (2 a) d (4 e)))

