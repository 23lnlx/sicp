(define (element-of-set? x set)
  (cond ((null? set ) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoint-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))
;;;Task
(define (union-set set1 set2)
  (if (null? set1) set2
      (union-set (cdr set1) (adjoint-set (car set1) set2))))

;;Tests
(define set1 (list 1 2 3 4))
(define set2 (list 2 3 4 5))
(element-of-set? 2 set1)
(intersection-set set1 set2)
(union-set set1 set2)