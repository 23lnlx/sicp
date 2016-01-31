(define (d i)
  (define (plus-2 n)
  (define (iter start n)
    (if (= n 1)
        start
        (iter (+ start 2) (- n 1))))
  (iter 1 n))
  (if (= i 1) 1 (plus-2 i)))

(define (n i x)
  (if (= i 1) x (* x x))) 

(define (cont-frac n1 d1 k x)
  (define (frac i)
    (if (= i k)
        (/ (n1 i x) (d1 i))
        (/ (n1 i x)
           (- (d1 i) (frac (+ i 1))))))
  (frac 1))

(define (tang x)
  (cont-frac n d 100 x))
