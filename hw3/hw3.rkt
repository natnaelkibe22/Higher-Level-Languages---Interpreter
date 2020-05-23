#lang errortrace racket
#|
    ===> PLEASE DO NOT DISTRIBUTE SOLUTIONS NOR TESTS PUBLICLY <===

   We ask that solutions be distributed only locally -- on paper, on a
   password-protected webpage, etc.

   Students are required to adhere to the University Policy on Academic
   Standards and Cheating, to the University Statement on Plagiarism and the
   Documentation of Written Work, and to the Code of Student Conduct as
   delineated in the catalog of Undergraduate Programs. The Code is available
   online at: http://www.umb.edu/life_on_campus/policies/code/
Name: Natnael Kibe
|#
(require rackunit)
(require "ast.rkt")
(provide (all-defined-out))

;; Exercise 1.a
(define p:empty 
  (delay empty)
)

;; Exercise 1.b
(define (p:empty? p) 
  (cond[(equal? empty (force p)) #t]
  [(empty? p) #t]
   [else #f]
   )
)
#|Exercise 1.c 
I think that it is impossible for x to be at the head of the promise list and
not get evaluated by a function or condition because if we try to implement a function like this
we might run into infinite loop problems cause we can't evaluate x as the rule. May be, it could be more likely that 
this might work if x was formed at the tail position of the promise list and make the head of the promise list be represented by l, 
but may be make the evaluation to be delayed or lazy evaluation so the evaluation of the head of the promise list will still has to take place 
before it can move on to the tail of the list. Therefore, it would likely not be possible for an evaluation of promise list l to be formed at the head without
evaluating x in one way or another. In which you could fix this issue would be to make x a delayed/lazy evaluation, this would allow for x to be the head and l
to be the tail of the promise list but we still have to evaluate x in order to stop the infinite loop of the promise list because for us in order to implement

there is an experimental code i implemented which might not work but it helped me understand if we could implement it or not!!!!
(define p:l (delay (cons (thunk 3) (cons (thunk 4) (cons (thunk 5) (list))))))
(define (p:cons x l)
  (delay (cons (thunk x) l))
)
(check-equal? ((p:first (p:cons (list 2 3) p:l))) (list 2 3))
!This code might not fulfill the requirement so the decision is still impossible to implement a promise list with x as the head and not being evaluated.
Final Conclusion, it is impossible despite I tried to do so using a small code!
|#
;; Exercise 1.d
(define (p:first l) 
   (car (force l))
)

;; Exercise 1.e
(define (p:rest l) 
   (cdr (force l))
)


;; Exercise 1.f
(define (p:append l1 l2) 
    (cond [(p:empty? l1) l2]
        [else (delay (cons (p:first l1) (p:append (p:rest l1) l2)
    ))])
)

;; Exercise 2.a
;; Auxiliary functions
(define (tree-left self) (first self))
(define (tree-value self) (second self))
(define (tree-right self) (third self))
#|
(define (bst->list self)
  (cond [(empty? self) self]
        [else
         (append
           (bst->list (tree-left self))
           (cons (tree-value self)
                 (bst->list (tree-right self))))]))
|#
(define (bst->p:list self) 
     (cond [(p:empty? self) p:empty]
        [else
         (p:append
              (bst->p:list (p:first self))
           (delay (cons (p:first (p:rest self))
                 (bst->p:list (tree-value (p:rest self))))))
        ])  
)
#|
Exercise 2.b
The difference between lazy evaluation and eager evaluation  is a difference of speed and for what purpose the programmer is trying to use which type of evaluation.
And the speed of the evaluation is also dependent on what purpose the evaluation is being used. For instance, what lazy evaluation does is just delaying the evaluation 
until it is needed for the later use, while eager evaluation evaluates and stores values inside the memory. The performance of lazy evaluation is faster because it avoids 
redundant caluclations. Lazy evaluation also has the feature to support the ability to build an infinite structures. 

We can use an easy function implementation to implement a function that adds two variable to another variable then adding the sum of those variables to the new evaluation, After we evaluate the 
eager and lazy evaluationa and when we check the memory usage we might observe that the lazy evaluation may have used less memory. The lazy may not have used up memory to store all its values, 
unlike the eager evaluation where it needs to access the memory. Therefore, Lazy evaluation does not need to access as much memory as eager evaluation which might take a slight smaller time 
for the lazy evaluation function to complete.
|#

;; Exercise 3
;; Auxiliary functions
(define (stream-get stream) (car stream))
(define (stream-next stream) ((cdr stream)))
(define (stream-foldl f a s) 
    (define (stream-foldl-helper accum f s)  
        (thunk (cons 
                accum
               (stream-foldl-helper (f (stream-get s) accum) f (stream-next s))
            )
     ))
    ((stream-foldl-helper a f s))
)

;; Exercise 4
(define (stream-skip n s) 
   (define (stream-skip-iter accu st)
       (thunk
          (cons 
             (+ accu (stream-get st))
             (stream-skip-iter accu (stream-next st))
          )
       )
   )
   ((stream-skip-iter n s))
)

;; Exercise 5
(struct r:bool (value) #:transparent)

(define (and-procedure)
    (lambda proc 
       (cond[(empty? proc) #t]
       [else (foldl (lambda (first second) (and second first)) (car proc) proc)]
    )
   )
)
(define (or-procedure)
    (lambda proc 
       (cond[(empty? proc) #t]
       [else (foldl (lambda (first second) (or second first)) (car proc) proc)]
    )
   )
)

(define (r:eval-builtin sym)
  (cond [(equal? sym '+) +]
        [(equal? sym '*) *]
        [(equal? sym '-) -]
        [(equal? sym '/) /]
        [(equal? sym 'and) (and-procedure)]
        [(equal? sym 'or) (or-procedure)]
        [else #f]))

(define (r:eval-exp exp)
  (cond
    [(r:bool? exp) (r:bool-value exp)]
    ; 1. When evaluating a number, just return that number
    [(r:number? exp) (r:number-value exp)]
    ; 2. When evaluating an arithmetic symbol,
    ;    return the respective arithmetic function
    [(r:variable? exp) (r:eval-builtin (r:variable-name exp))]
    ; 3. When evaluating a function call evaluate each expression and apply
    ;    the first expression to remaining ones


[(r:apply? exp)
 (apply (r:eval-exp (r:apply-func exp))
    (cond [(list? (r:apply-args exp))
    (map r:eval-exp (r:apply-args exp))
    ]
    [else 
    (r:eval-exp (first (r:apply-args exp)))
    (r:eval-exp (second (r:apply-args exp)))]))
  ]
     [else (error "Unknown expression:" exp)]))

