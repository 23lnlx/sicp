(define (last-pair l) 
  (if (null? (cdr l))
            (car l)
            (last-pair (cdr l))))

(define (reverse l)
  (if (null? l)
      l
      (append (reverse (cdr l)) (list (car l)))))