#lang racket
#|
            ######################################################
            ###  PLEASE DO NOT DISTRIBUTE TEST CASES PUBLICLY  ###
            ###         SHARING IN THE FORUM ENCOURAGED        ###
            ######################################################

  You are encouraged to share your test cases in the course forum, but please
  do not share your test cases publicly (eg, GitHub), as that stops future
  students from learning how to write test cases, which is a crucial part of
  this course.
|#
(require rackunit)
(require "ast.rkt")
(require "hw2.rkt")
(define (cell-get c) (c (list)))
(define (cell-set c x) (c (list x)))

; 1.a
(define w1 (rw-cell 10))
(check-equal? 10 (cell-get w1))
(define w2 (cell-set w1 20))
(check-equal? 20 (cell-get w2))

; 1.b
(define r1 (ro-cell 10))
(check-equal? 10 (cell-get r1))
(define r2 (cell-set r1 20))
(check-equal? 10 (cell-get r2))

; 2
(check-equal? (list 1 0 2 0 3) (intersperse (list 1 2 3) 0))

; 3.a
(check-equal? (cons 0 10) (find (lambda (idx elem) #t) (list 10 20 30)))
(check-equal? #f (find (lambda (idx elem) #f) (list 10 20 30)))

; 3.b
(check-true (member 20 (list 10 20 30)))
(check-false (member 40 (list 10 20 30)))

; 3.c
(check-equal? 1 (index-of (list 10 20 30) 20))
(check-equal? #f (index-of (list 10 20 30) 40))

; 4
(define (f x y z w)
  (+ x y z w))
(define g (uncurry (curry f)))
(check-equal? 10 (g (list 1 2 3 4)))

; 5
(check-equal? (parse-ast 'x) (r:variable 'x))
(check-equal? (parse-ast '10) (r:number 10))
(check-equal?
  (parse-ast '(lambda (x) x))
  (r:lambda (list (r:variable 'x)) (list (r:variable 'x))))
(check-equal?
  (parse-ast '(define (f y) (+ y 10)))
  (r:define
    (r:variable 'f)
    (r:lambda
      (list (r:variable 'y))
      (list (r:apply (r:variable '+) (list (r:variable 'y) (r:number 10)))))))
