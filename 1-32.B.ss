(define (accum combiner null-val term a next-v b filter)
    (if  (> a b)
        null-val
        (combiner 
         (if (filter a) (term a) null-val)
          (accum combiner null-val 
                 term (next-v a) next-v b filter))))


(define (inc x) (+ x 1))
(define (id x) x)

(define (next a)
  (if (= a 2) 3 (+ a 2)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (square a)
  (* a a))

(define (prime? n)
  (= (smallest-divisor n) n))

;(define (gcd a b)
;  (if (= b 0)
;      a
;      (gcd b (remainder a b))))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (product n)
  (define (prime-n i)
    (= (gcd n i) 1))
  (accum + 0 id 1 inc n prime-n))

(define (f g) (g 2))