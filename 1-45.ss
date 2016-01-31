(define tolerance 0.000001)

(define (compose foo goo)
  (lambda (x) (foo (goo x))))

(define (repeated foo times)
  (if (= times 1) 
      (lambda (x) (foo x))
      (compose foo (repeated foo (- times 1)))))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
     ; (newline)
      ;(display next)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average-damp f)
  (lambda (x) (/ (+ x (f x))
                 2)))

(define (aver-n foo times)
  ((repeated average-damp times) foo))
(define (pow y n)
  (define (iter a y n)
    (if (= n 0) a
        (iter (* a y) y (- n 1))))
  (iter 1 y n))

(define (root n x)
  (fixed-point (aver-n (lambda (y) (/ x (pow y (- n 1))))  n)
               1.))