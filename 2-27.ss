(define (reverse l)
  (cond ((null? l) l)
        ((not (pair? (car l))) (append (reverse (cdr l)) (list (car l))))
        (else
      (append (reverse (cdr l)) (reverse (car l))))))