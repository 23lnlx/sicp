(define (fringe l)
  (cond ((null? l) l)
        ((not (pair? l)) (display l))
        (else
         (fringe (car l))
         (fringe (cdr l)) 
       )))