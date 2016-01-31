#lang racket
(define (pascal power n-elem)
  (cond ((= n-elem 0) 1)
        ((= n-elem power) 1)
        (else (+
               (pascal (- power 1) (- n-elem 1))
               (pascal (- power 1) n-elem)))))