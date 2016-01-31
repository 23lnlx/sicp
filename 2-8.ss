(define (add-interval x y)
  (make-interval (+ (low x) (low y))
                 (+ (up x) (up y))))
(define (zero-inc? interval)
  (and 
   (not (> (low interval) 0))
   (not (< (up interval) 0))))

(define (mul-interval x y)
  (let ((p1 (* (low x) (low y)))
        (p2 (* (low x) (up y)))
        (p3 (* (up x) (low y)))
        (p4 (* (up x) (up y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (and (not (zero-inc? x))
           (not (zero-inc? y)))
  (mul-interval x 
                (make-interval (/ 1. (up y))
                               (/ 1. (low y))))
  (display "INCORRECT INTERVAL")))
  
(define (make-interval a b) (cons a b))
(define (low a) (car a))
(define (up a) (cdr a))

(define (diff-interval x y)
  (make-interval (- (low x) (up y))
                 (- (up x) (low y))))

(define (width interval)
  (/ (- (up interval) (low interval)) 2))


(define (make-c-w c w)
  (make-interval (-cw) (+ c w)))

(define (center i)
  (/ (+ (low i) (up i)) 2))
(define (percent i)
  (/ (width i) (center i)))

(define (make-c-per c per)
  (make-interval (- c (* c per))
                 (+ c (* c per))))
               
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one 
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))
               
               