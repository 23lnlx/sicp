(define (double foo)
  (lambda (x) (foo (foo x))))

(define (inc x)
  (+ 1 x))
(define (square x)
  (* x x))

(define (compose foo goo)
  (lambda (x) (foo (goo x))))

(define (repeated foo times)
  (if (= times 1) 
      (lambda (x) (foo x))
      (repeated (compose foo foo) (- times 1))))
  
(define (rep foo times)
  (lambda (x) 
    (define (iter a times)
      (if (= times 0) 
          a
          (iter (foo a) (- times 1))))
    (iter x times)))
    
  
  