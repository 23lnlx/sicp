#lang planet neil/sicp
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))


(define (lookup given-key records)
  (cond ((null? records) #f)
        ((= given-key (entry records)) (entry records))
        ((< given-key (entry records))
         (lookup given-key (left-branch records)))
        (else
         (lookup given-key (left-branch records)))))