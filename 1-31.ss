(define (even? x)
  (= (remainder x 2) 0))

(define (prod term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) 
              (* result (term a)))))
  (iter a 1))

(define (inc n)
   (+ n 1))
(define (div n) 
  (if (even? n)
      (/ (+ n 2) (+ n 1))
      (/ (+ n 1) (+ n 2))))