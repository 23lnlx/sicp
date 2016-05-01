#lang planet neil/sicp
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))
;; Decode
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "плохой бит -- CHOOSE-BRANCH" bit))))
;;Building
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))
(define (sort-leaves leaves)
  (if (null? leaves)
      '()
      (let ((leaf (car leaves)))
        (adjoin-set leaf
                    (sort-leaves (cdr leaves))))))

;;Task0
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))
;(cadr sample-tree)
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
;;Task1
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (define (encoder symbol tree result)
    (let ((right-tree (right-branch tree))
          (left-tree (left-branch tree)))
      
      (cond ((leaf? tree) result)
            ((element-of-set? symbol (symbols right-tree))
             (encoder symbol right-tree (append result (list 1))))
            ((element-of-set? symbol (symbols left-tree))
             (encoder symbol left-tree (append result (list 0))))
            (else (error "Symbol not found" symbol)))))
  (encoder symbol tree '()))

;;Task2
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaves)
  (let ((leaf-1 (car leaves))
        (leaf-2 (cadr leaves))
        (other-leaves (cddr leaves)))
    (if (null? other-leaves)
        (make-code-tree leaf-1 leaf-2)
        (successive-merge
         (sort-leaves (cons (make-code-tree leaf-1 leaf-2)
                            other-leaves))))))
                            

        
        
      
      
;;Result
(display (decode sample-message sample-tree))
(display (encode (decode sample-message sample-tree) sample-tree))
(display (generate-huffman-tree (list (list 'A 4)
                              (list 'B 2)
                              (list 'C 1)
                              (list 'D 1))))