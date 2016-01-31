#lang racket
(define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x)
        x)))

(define (improve guess x)
    (average (/ x (square guess)) (* 2 guess)))

(define (average x y)
    (/ (+ x y) 3))

(define (good-enough? guess x)
    (< 
     (/ 
      (abs (- guess (improve guess x)))
       guess)    
    0.001))

(define (sqrt x)
    (sqrt-iter 1.0 x))

(define (square x)
  (* x x))