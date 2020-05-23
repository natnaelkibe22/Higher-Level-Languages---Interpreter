#lang racket
;; Please, do not remove this line and do not change the function names,
;; otherwise the grader will not work and your submission will get 0 points.
(provide (all-defined-out))

(define ex1 (+ (- 6 (* 9 14)) (/ (- 12 15) 6)))
(define ex2 (list 
(+ (- 6 (* 9 14)) (/ (- 12 15) 6))
(+ (- 6 126) (/ (- 12 15) 6))
(+ -120 (/ (- 12 15) 6))
(+ -120 (/ -3 6))
(+ -120 -1/2)
-241/2))
(define (ex3 x y)
  (>
  (+ x (+ x y))
  (+ (* 11 x) (* y 3)) 
  )
)
;; Constructs a tree from two trees and a value
(define (tree left value right) 
   (list left value right)
)
;; Constructs a tree with a single node
(define (tree-leaf value) (list (list) value (list)))

;; Accessors
(define (tree-left self) (first self))
(define (tree-value self) (second self))
(define (tree-right self) (third self))

;; Copies the source and updates one of the fields
(define (tree-set-value self value) (list (tree-left self) value (tree-right self)))
(define (tree-set-left self left) (list left (tree-value self) (tree-right self)))
(define (tree-set-right self right) (list (tree-left self) (tree-value self) right))

;; Function that inserts a value in a BST
(define (bst-insert self value) 
(cond[(equal? self (list)) (list (list) value (list))]
[(equal? value (tree-value self)) (tree-set-value self value)]
[(< value (tree-value self)) (tree-set-left self (bst-insert (tree-left self) value))] 
[else (tree-set-right self (bst-insert (tree-right self) value))]
)) 

;; lambda
(define (lambda? node) 
    (and 
    (list? node)
    (> (length node) 2)
    (equal? 'lambda (first node))
    (not (empty? node))
    (list? (second node))
    (andmap symbol? (second node))
    )
)
(define (lambda-params node) 
    (second node)
)
(define (lambda-body node) 
    (rest (rest node))
)

;; apply
(define (apply? l) 
  (and 
  (list? l)
  (>= (length l) 1)
  (not (equal? 'lambda (first l)))
  )
   
)
(define (apply-func node) 
   (first node)
)
(define (apply-args node) 
   (rest node)
)
;; define
(define (define? node) 
 
 (cond[(equal? (and 
     (list? node)
     (= (length node) 3)
     (equal? 'define (first node))
     (symbol? (second node))
     ) #t)]
     [(equal? (and 
    (list? node)
    (> (length node) 2)
    (equal? 'define (first node))
    (list? (second node))
    (not (empty? (second node)))
    (andmap symbol? (second node))
    ) #t)]
    [else #f])
)
(define? (quote (define x x)))
(define (define-basic? node)
     (and 
     (list? node)
     (= (length node) 3)
     (equal? 'define (first node))
     (symbol? (second node))
     )
)
(define (define-func? node) 
    (and 
    (list? node)
    (> (length node) 2)
    (equal? 'define (first node))
    (list? (second node))
    (not (empty? (second node)))
    (andmap symbol? (second node))
    )
)



