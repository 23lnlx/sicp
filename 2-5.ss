(define (pow a b) 
  (cond ((= b 0) 1)
        ((= b 1) a)
        (else (pow (* a a) (- b 1)))))

(define (how-many num prod alt)
  (define (iter times prod)
    (if (or (= prod 1) 
            (> (remainder prod num) 0)) times
                          (iter (+ 1 times) (/ prod num))))
  (if (= (remainder prod num) 0) 
      (iter 0 prod)
      0))
        

(define (cons a b)
  (* (pow 2 a)
     (pow 3 b)))

(define (car z)
  (how-many 2 z 3))
(define (cdr z)
  (how-many 3 z 2))