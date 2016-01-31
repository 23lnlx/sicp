(define (double x)
  (* x x))

(define (squre-l items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (double (car things))
                    answer))))
  (iter items nil))