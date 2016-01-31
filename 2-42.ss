(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op init sequence)
  (if (null? sequence)
      init
      (op (car sequence)
          (accumulate op init (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))
;;;;;;;;;
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (queens board-size) 
   (define (queen-cols k) 
     (if (= k 0) 
         (list empty-board) 
         (filter 
          ; This checks if the current queen is safe from the rest of the queens 
          (lambda (positions) (safe? k positions)) 
          (flatmap 
           (lambda (rest-of-queens) 
             (map (lambda (new-row) 
                    (adjoint-position new-row k rest-of-queens)) 
                  (enumerate-interval 1 board-size))) 
           (queen-cols (- k 1)))))) 
   (queen-cols board-size)) 

(define empty-board nil)

(define (adjoint-position new-row k rest-of-queens)
  (cons new-row rest-of-queens))

(define (safe? k positions)
  (define last-queen (car positions))
  (define (iter top bot remain)
    (cond ((null? remain) #t)
          ((or (= (car remain) last-queen)
               (= (car remain) top)
               (= (car remain) bot))
           #f)
          (else
           (iter (- top 1) (+ bot 1) (cdr remain)))))
  (iter (- last-queen 1) (+ last-queen 1) (cdr positions)))
           
(queens 8)