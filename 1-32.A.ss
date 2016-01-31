(define (accum combiner term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a 0))

(define (inc x) (+ x 1))
(define (id x) x)