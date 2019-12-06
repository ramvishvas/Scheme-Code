(define (sum lst)
     (if(null? lst)
        0
        (+ (cadar lst) (sum (cdr lst)))))

(define (length lst)
    (if(null? lst)
       0
       (+ 1 (length (cdr lst)))))
       
(define (average lst)
     (/ (sum lst) (length lst)))
     
(define (above_count lst)
    (if(null? lst)
       0
       (if(> (cadar lst) (average lst))
          (+ 1 (above_count (cdr lst)))
          (above_count (cdr lst)))))
      
(define (%above_count lst)
   (if(null? lst)
        0
       (* (/ (above_count lst) (length lst)) 100)))
