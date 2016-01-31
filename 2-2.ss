(define (make-seg xy1 xy2) (cons xy1 xy2))
(define (start-point seg) (car seg))
(define (end-point seg) (cdr seg))

(define (make-point x y) (cons x y))
(define (x-point point) (car point))
(define (y-point point) (cdr point))

(define (mipoint seg)
  (make-point 
   (/ (+ (x-point (start-point seg)) 
         (x-point (end-point seg))) 
      2)
   (/ (+ (y-point (start-point seg))
         (y-point (end-point seg)))
      2)))

(define (print-p p)
    (newline)
    (display "(")
    (display (x-point p))
    (display ",")
    (display (y-point p))
    (display ")"))
