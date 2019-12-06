(define merg
(lambda (ls1 ls2)
(cond 
((null? ls2) ls1)
((null? ls1) ls2)
((= (car ls1) (car ls2)) (cons (car ls1) (merg (cdr ls1) (cdr ls2))))
((< (car ls1) (car ls2)) (cons (car ls1) (merg (cdr ls1) ls2)))
((> (car ls1) (car ls2)) (cons (car ls2) (merg ls1 (cdr ls2))))

(define (merge l1 l2) 
(cond 
((null? l2) l1) 
((< (car l1) (car l2)) (cons (car l1) (merge (cdr l1) l2))) 
((> (car l1) (car l2)) (cons (car l2) (merge l1 (cdr l2))))
))

(define (ser n) 
(if(= n 0)
0
(+ (exp n) (ser (- n 1))
