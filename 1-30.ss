(define (sum term a next b)
  (define (iter a result)
    (if (= a b)
        (+ result (term b))
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (inc x) (+ x 1))
(define (id x) x)