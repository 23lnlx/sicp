#lang racket
(define (halve x)
  (/ x 2))

(define (double x)
  (+ x x))

(define (even? x)
  (= (remainder x 2) 0))
(define (* a b)
  (iter 0 a b))

(define (iter sum a b)
  (cond ((= b 0) sum)
        ((even? b)  (iter sum (double a) (halve b)))
        (else (iter (+ sum a) a (- b 1)))))