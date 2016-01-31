(define (square x)
  (* x x))

(define (even? x)
  (= (remainder x 2) 0))

(define (sqrmod-test x m)
  (define (sqrmod x m)
    (remainder (square x) m))
  
  (if
   (and (= (sqrmod x m) 1)
        (not (= x 1)) 
        (not (= (x (- m 1)))))
   0
   (sqrmod x m)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (sqrmod-test (expmod base (/ exp 2) m) m))
        (else 
         (remainder (* base (expmod base (- exp 1) m))
                    m))))
  
(define (true-prime? n)
  (define (try-it a)
    (define (show modi)
      (newline)
      (display modi)
      (= modi 0))
    (show (expmod a n n)))
  (try-it  (random (- n 1))))