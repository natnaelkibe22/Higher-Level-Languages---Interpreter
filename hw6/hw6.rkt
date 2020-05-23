#lang errortrace racket
#|
    ===> PLEASE DO NOT DISTRIBUTE THE SOLUTIONS PUBLICLY <===

   We ask that solutions be distributed only locally -- on paper, on a
   password-protected webpage, etc.

   Students are required to adhere to the University Policy on Academic
   Standards and Cheating, to the University Statement on Plagiarism and the
   Documentation of Written Work, and to the Code of Student Conduct as
   delineated in the catalog of Undergraduate Programs. The Code is available
   online at:

   https://www.umb.edu/life_on_campus/dean_of_students/student_conduct

|#
(require racket/set)
(require "hw6-util.rkt")
(provide frame-refs mem-mark mem-sweep mlist mapply)

;;;;;;;;;;;;;;;
;; Exercise 1

(define/contract (frame-refs frm)
  (-> frame? set?)

    (define d (set))
   
     (define (proc l)
         (cond 
         [(empty? l) (set)]
         [(d:closure? (first l)) 
              (cond[(not (empty? l))
              (set-add (proc (rest l)) (d:closure-env (first l)))
              ])
         ]
         [else (set-add (proc (rest l)) d)
         ]
     )
     )
    (cond[(frame-parent frm) 
    (define d (set-add (proc (frame-values frm)) (frame-parent frm)))
    (set-subtract d (set (set)))
     ]
    [else (set-subtract (proc (frame-values frm)) (set (set)))]
    )
  )
;;;;;;;;;;;;;;;
;; Exercise 2

(define/contract (mem-mark contained mem env)
  (-> (-> any/c set?) heap? handle? set?)
     (define (mem-mark-helper env)
        (cond 
        [(not (frame-parent (heap-get mem env))) (set env)]
        [(or (equal? (frame-parent (heap-get mem env)) env)) (contained (heap-get mem env))]
         [else
          (set-add (set-union (contained (heap-get mem env)) (mem-mark-helper (frame-parent (heap-get mem env)))) env)
         ]
         )
     )
     (mem-mark-helper env)
)
;;;;;;;;;;;;;;;
;; Exercise 3

(define/contract (mem-sweep mem to-keep)
  (-> heap? set? heap?)
  (heap-filter (lambda (proc val) (set-member? to-keep proc)) mem)
  )

;;;;;;;;;;;;;;;
;; Exercise 4

(define (mlist bind pure args)
  (define (mlist-helper accum lst)
     (cond [(empty? lst) (pure accum)] 
     [else (bind (first lst) (lambda(l) (mlist-helper (append accum (list l)) (rest lst))))
     ])
   ) 
  (mlist-helper (list) args)
)
    

;;;;;;;;;;;;;;;
;; Exercise 5

(define (mapply bind pure f . args)
  (mlist bind (lambda (arg) (pure (apply f arg))) args)
)

;;;;;;;;;;;;;;;
;; Exercise 6 (MANUALLY GRADED)
#|
PLEASE REPLACE THIS TEXT BY YOU ANSWER.
YOU MAY USE MULTIPLE LINES.
If the counting algorithm is faulty or the reference counting algorithm overflows and set back to zero, the memory management system wouldn't 
work properly since when the counter sets to zero, it can free the memory space which could lead to an incorrect reference to a variable. 
Sometime setting the counter to zero can fire or execute a different reference of memory. As we defined that we need memory management to remove
unneded data but if we reclaim the memory too soon it might create confusions with references. 
The soundness of the memory management changes because the memory can be allocated to a different place too early. I don't think it would 
affect the completeness of the memory management since the memory is being checked if it is cleaned or not. But our algorithm would still work
partially though since we don't know how much time or how often the overflaw occurs. But when it occurs, it would affect the correctness of the answers like
parameters and values and functions and variable references since we are reclaiming memory too soon due to the overflaw. 
|#