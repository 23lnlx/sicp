(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponent? exp)
         (make-product (exponent exp)
                       (make-product
                        (make-exponent (base exp) (- (exponent exp) 1))
                        (deriv (base exp) var))))
                       
        (else 
         (error "Unknown type" exp))))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

;;sum
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) 
    (if (null? (cdddr s))
      (caddr s)
      (make-sum (addend (cdr s))
                (augend (cdr s)))))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

;;prod
(define (product? p) (and (pair? p) (eq? (car p) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) 
      (if (null? (cdddr p))
      (caddr p)
      (make-sum (multiplier (cdr p))
                (multiplicand (cdr p)))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

;; Task
;'(** u n) == u**n
(define (exponent? exp)
  (and (pair? exp) (number? (exponent exp)) (eq? (car exp) '**)))
(define (exponent exp) (caddr exp))
(define (base exp) (cadr exp))

(define (make-exponent b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        (else (list '** b e))))

;;;Try
(display (deriv '(+ x 3 (* y x) x) 'x))
(deriv '(* x y) 'x)
(display (deriv '(* (* x y) (+ x 3) (* x y z)) 'x))
;(newline)
;(display (deriv '(** (* x y) 10) 'x))