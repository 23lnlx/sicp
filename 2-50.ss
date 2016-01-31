(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

;;
;(memq 'apple '(ban (apple pl) xp))


;(list 'a 'b 'c)
;(list (list 'george))
;(cdr '((x1 x2) (y1 y2)))
;(cadr '((x1 x2) (y1 y2)))
;(pair? (car '(a short list)))
;(memq 'red '((red shoes) (blue socks)))
;(memq 'red '(red shoes blue socks))

(define (equal? l1 l2)
  (display l1)
  (cond 
    ((and (null? l1) 
          (null? l2)) true)
    ((or (null? l1)
         (null? l2)) false)
    ((and (pair? (car l1)) 
          (pair? (car l2))) (equal? (car l1) (car l2)))
    ((not (eq? (car l1) (car l2))) false)
    (else (equal? (cdr l1) (cdr l2)))))
;;Test
(equal? '(this is a) '(this is))
        
(define (iter n p)
  (