(define (even? x)
  (= (remainder x 2) 0))

(define (coub x)
  (* x x x))

(define (sum term a next b n iter-par)
  (if (> iter-par n)
      0
      (+ (cond ((= iter-par n) (term a))
               ((= iter-par 0) (term a))
               ((even? iter-par) (* 2 (term a)))
               (else (* 4 (term a))))
         (sum term (next a) next b n (+ iter-par 1)))))

(define (h a b n)
  (/ (- b a) n))

(define (simps f a b n)
  (define (next-a x) (+ x (h a b n)))
  (* (sum f a next-a b n 0)
     (/ (h a b n) 3)))