#lang racket
#|
            #####################################################
            ###  PLEASE DO NOT DISTRIBUTE SOLUTIONS PUBLICLY  ###
            #####################################################

  Copy your solution of HW1 as file "hw1.rkt". The file should be in the same
  directory as "hw2.rkt" and "ast.rkt".
|#
(require "ast.rkt")
(require "hw1.rkt")
(require rackunit)
(provide (all-defined-out))
;; ^^^^^ DO NOT CHANGE ANY CODE ABOVE THIS LINE ^^^^^
;Name: Natnael Kibe        class: 450            HW2

;; Exercise 1.a: Read-write cell
;; Solution has 3 lines.
(define (rw-cell x) 
     (define var (lambda (d)
         (cond[(and (list? d) (null? d)) x]
         [else (rw-cell (last d))]
         )))
    var
  )


;; Exercise 1.b: Read-only cell
;; Solution has 4 lines.
(define (ro-cell x) 
     (define var (lambda (d)
         (cond[(and (list? d) (null? d)) x]
         [else (ro-cell x)]
         )))
    var
)

;; Exercise 2: Interperse
;; Solution has 11 lines.
(define (intersperse l v)
     (define (intersperse-helper accum l) 
     (cond [(equal? (length l) 1) (accum l)]
       [(empty? l) (list)]
       [else (intersperse-helper (lambda (x) (accum (cons (first l) (cons v x)))) (rest l))]))  
      (intersperse-helper (lambda (x) x) l))

;; Exercise 3.a: Generic find
;; Solution has 7 lines.
(define (find pred l) 
   ;accum: index
   (define (find-helper accum l)
       (cond[(= (length l) 0) #f]
         [(pred accum (first l)) 
         (cons accum (first l))
         ]
         [else (find-helper (+ 1 accum) (rest l))]))
    (find-helper 0 l)
)

;; Exercise 3.b: Member using find
;; Solution has 3 lines.
(define (member x l) 
     (pair? (find (lambda (idx elem) (equal? elem x)) l))
)


;; Exercise 3.c: index-of using find
;; Solution has 4 lines.
(define (index-of l x) 
   (cond [(= (length l) 0) #f]
   [(pair? (find (lambda (idx elem) (equal? elem x)) l)) 
   (car (find (lambda (idx elem) (equal? elem x)) l))
   ]
   [else #f]
   )
)

;; Exercise 4: uncurry, tail-recursive
;; Solution has 8 lines.
(define (uncurry f) 
   (lambda (l)
        (define uncurry-start (lambda (l accum)
           (cond [(= (length l) 0) (accum)]
              [(= (length l) 1) (accum (first l))]
              [else (uncurry-start (rest l) (accum (first l)))]
          )
        )
        )
        (uncurry-start l f)
   )
)

;; Exercise 5: Parse a quoted AST
;; Solution has 26 lines.
(define (parse-ast node)
  (define (make-define-func node) 
    (r:define (make-variable (first (second node))) (r:lambda (map make-variable (rest (second node))) (map parse-ast (rest (rest node)))))
  )
  (define (make-define-basic node) 
    (r:define (parse-ast (second node))  (parse-ast (first (rest (rest node)))))
    
  )
  (define (make-lambda node) 
    (r:lambda (map make-variable (second node)) (map parse-ast (rest (rest node))))
  )
  (define (make-apply node) 
    (cond[(empty? node) (list)]
    [(apply? node)
      (r:apply (parse-ast (first node)) (map parse-ast (rest node)))
    ]
    [else 
         (map parse-ast node)
    ]
    )
  )
  (define (make-number node) 
       (r:number node)
  )
  (define (make-variable node) 
       (r:variable node)
  )

  (cond
    [(define-basic? node) (make-define-basic node)]
    [(define-func? node) (make-define-func node)]
    [(symbol? node) (make-variable node)]
    [(real? node) (make-number node)]
    [(lambda? node) (make-lambda node)]
    [else (make-apply node)]))
