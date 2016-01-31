(define (for-each fact l)
  (cond ((null? l) nil)
        (else
          (fact (car l))
          (for-each fact (cdr l)))))