#lang racket
(define (F x)
  (cond ((< x 3) x)
        (else (+ 
               (F (- x 1)) 
               (F (- x 2)) 
               (F (- x 3))))))

(define (f x)  
  (define (iter-f a b c x)
    (if (= x 0) 
        a
        (iter-f b c (+ a b c) (- x 1))))
  (iter-f 0 1 2 x))