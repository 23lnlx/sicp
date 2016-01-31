#lang racket
(define (halve x)
  (/ x 2))

(define (double x)
  (+ x x))

(define (even? x)
  (= (remainder x 2) 0))

;(define (* a b)
;  (iter 0 a b))

(define (* a b)
  (cond ((= b 0) 0)
        ((= b 1) a)
        ((even? b)  (* (double a) (halve b)))
        (else (+ a (* a (- b 1))))))