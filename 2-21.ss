(define (double x)
  (* x x))

(define (square-l1 items)
  (if (null? items)
      nil
      (cons (double (car items)) (square-l1 (cdr items)))))

(define (square-l2 items)
  (map double items))