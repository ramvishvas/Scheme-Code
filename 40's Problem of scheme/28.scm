; Problem 28(**):
;
; Sorting a list of lists according to length of sublists
;
; a) we suppose that a list contains elements that are lists themselves. The
; objective is to sort the elements of this list according to their length.
; E.g. short lists first, longer lists later, or vice versa.
;
; Example:
;
; (lsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))
;===> ((o) (d e) (d e) (m n) (a b c) (f g h) (i j k l))
;
; b) Again, we suppose that a list contains elements that are lists
; themselves. But this time the object is to sort the elements of this list
; according to their length frequency; i.e, in the default, where sorting is
; done ascendingly, lists with rare lengths are placed first, others with a
; more frequent length come later.
;
; Example:
;
; (lfsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))
; ===> ((i j k l) (o) (a b c) (f g h) (d e) (d e) (m n))
;
; Note that in the above example, the first two lists in the result have
; length 4 and 1, both lengths appear just once. the third and forth list have
; length 3 which appears twice(there are two list of this length). And
; finally, the last three lists have length 2. This is the most frequent
; length.

; Normally we should use build in `sort` function, here we'll implement a
; `quicksort` just for demonstration.
; Note that this implementation is intuitive but inefficient.
; 
; a list (cur ...) is parted into (...< cur...) cur (... > cur ...)
; and do it recursively.
(define (qsort lst cmp)
  (if (null? lst)
    '()
    (let part ((cur (car lst)) (lst (cdr lst)) (first '()) (second '()))
      (cond
        ((null? lst) (append (qsort first cmp)
                             (list cur)
                             (qsort second cmp)))
        ((cmp (car lst) cur) 
         (part cur (cdr lst) (cons (car lst) first) second))
        (else
          (part cur (cdr lst) first (cons (car lst) second)))))))

; test of qsort.
(qsort '(2 4 6 1 5 8 3 4 1) <)

; Problem a)
; using build in `sort` function:
(define (lsort lst)
  (sort lst (lambda (lst1 lst2) (< (length lst1) (length lst2)))))

; test
(lsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))

; Problem b)
; First, we make a histogram for all lists.
; Later, we check the histogram for frequency.

; we use a hash table for high performance

(define (make-histogram lst)
  (let ((hist (make-hash-table 'equal?)))
    (map (lambda (x)
           (let ((value (hash-table-get hist x 0)))
             (hash-table-put! hist x (+ value 1))))
         lst)
    hist))

; test hash-table
(define *ht* (make-hash-table 'equal?))
(hash-table-put! *ht* '(a) 10)
(hash-table-get *ht* '(b) #t)

(define (lfsort lst)
  (let ((hist (make-histogram lst)))
    (let ((key (lambda (x) 
                 (hash-table-get hist x 0))))
      (sort lst (lambda (x y) (< (key x) (key y)))))))

; test
;(lfsort '((a b c) (d e) (f g h) (d e) (i j k l) (m n) (o)))
