(define (frame-coord-map frame)
  (lambda (v)
    (add-vect 
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))

(define (add-vect v1 v2)
  (make-vect 
   (+ (xcor-vect v1) (xcor-vect v2))
   (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect 
   (- (xcor-vect v1) (xcor-vect v2))
   (- (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect s v1)
  (make-vect 
   (* s (xcor-vect v1))
   (* s (ycor-vect v1))))

;;; Test
(define a-frame (make-vect 1 1))
((frame-coord-map a-frame) (make-vect 0 0))
