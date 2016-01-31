(define (fixed-point f first-guess)
  (define (g? guess)
    (< (abs (- guess (f guess))) 0.00001))
  (define (im guess)
    (f guess))
  ((iterative-improve g? im) first-guess))

(define (square x)
  (* x x))

(define (average x y)
  (/ (+ x y) 2))

(define (iterative-improve good? improvement)
  (lambda (guessl) 
    (define (try guess)
      (if (good? guess)
          guess
          (try (improvement guess))))
    (try guessl)))

(define (sqrt x)
  (define (g? guess)
    (< (abs (- (square guess) x)) 0.00001))
  (define (im guess)
    (average guess (/ x guess)))
  ((iterative-improve g? im) 1.))