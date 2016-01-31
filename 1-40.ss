(define tolerance 0.000001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (newline)
      (display next)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define dx 0.00001)

(define (D foo)
  (lambda (x) (/ (- (foo (+ x dx)) (foo x))
                 dx)))

(define (newton-transform foo)
  (lambda (x) (- x (/ (foo x) ((D foo) x)))))

(define (newton-method foo guess)
  (fixed-point (newton-transform foo) guess))

(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))