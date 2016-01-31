
(define (iter-expt a b n)
  (cond ((= n 0) a)
        ((even? n) (iter-expt  a  (* b b) (/ n 2)))
        (else (iter-expt (* a b) b (- n 1)))))

(define (expt b n)
  (iter-expt 1 b n))
  
(expt 3 10)
