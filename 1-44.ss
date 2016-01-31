(define (inc x)
  (+ 1 x))
(define (square x)
  (* x x))
(define dx 0.000001)


(define (compose foo goo)
  (lambda (x) (foo (goo x))))

(define (repeated foo times)
  (if (= times 1) 
      (lambda (x) (foo x))
      (compose foo (repeated foo (- times 1)))))

;(define (repp foo times)
;  (let ((y (lambda (x) (foo x))))
;    (define (iter res 

(define (smooth foo)
  (lambda (x) (/ (+ (foo (- x dx)) 
                    (foo x) 
                    (foo (+ x dx))) 
                 3)))

(define (smooth-n foo times)
  ((repeated smooth times) foo))
  