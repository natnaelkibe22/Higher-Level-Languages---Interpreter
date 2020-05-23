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
(require "hw3.rkt")
;; Exercise 1.a
(check-equal? empty (force p:empty))

;; Exercise 1.b
(check-true (p:empty? p:empty))
(check-false (p:empty? 10))

;; Exercise 1.d
(check-equal? (p:first (delay (cons 1 p:empty))) 1)

;; Exercise 1.e
(check-equal? (p:rest (delay (cons 1 p:empty))) p:empty)

;; Exercise 3
(define (naturals)
  (define (naturals-iter n)
    (thunk
      (cons n (naturals-iter (+ n 1)))))
  ((naturals-iter 0)))

(define (ex1)
  (define s (stream-foldl cons empty (naturals)))
  (check-equal? (stream-get s) empty)
  (check-equal? (stream-get (stream-next s)) (list 0))
  (check-equal? (stream-get (stream-next (stream-next s))) (list 1 0))
  (check-equal? (stream-get (stream-next (stream-next (stream-next s)))) (list 2 1 0)))
(ex1)

(define (ex2)
  (define s (stream-skip 10 (naturals)))
  (check-equal? (stream-get s) 10)
  (check-equal? (stream-get (stream-next s)) 11)
  (check-equal? (stream-get (stream-next (stream-next s))) 12))
(ex2)

;; Exercise 5

(check-equal? (r:eval-builtin '+) +)
(check-equal? (r:eval-builtin '-) -)
(check-equal? (r:eval-builtin '/) /)
(check-equal? (r:eval-builtin '*) *)
(check-equal? (r:eval-builtin 'and) and-n)
(check-equal? (r:eval-builtin 'foo) #f)

;; Exercise 5.b
(check-equal? (r:eval-exp (r:bool #t)) #t)
(check-equal? (r:eval-exp (r:bool #f)) #f)
(check-equal? (r:eval-exp (r:variable '+)) +)
;; Exercise 5.c
(check-equal? (r:eval-exp (r:variable 'and)) and-n)
(check-true (r:eval-exp (r:apply (r:variable 'and) (list (r:bool #t) (r:bool #t)))))
(check-false (r:eval-exp (r:apply (r:variable 'and) (list (r:bool #t) (r:bool #f)))))
(check-equal?
  (r:eval-exp
    (r:apply (r:variable 'and)
      (list
        (r:bool #t)
        (r:bool #t)
        (r:apply (r:variable '+) (list (r:number 2) (r:number 3))))))
  5)

(check-equal? (r:eval-exp (r:apply (r:variable '+) (list (r:number 1) (r:number 2) (r:number 3)))) 6)
(check-equal?
  (r:eval-exp
    (r:apply (r:variable 'and)
      (list (r:bool #t) (r:number 2) (r:number 3))))
  3)

(check-true (r:eval-exp (r:apply (r:variable 'and) (list (r:bool #t) (r:bool #t)))))
(check-false (r:eval-exp (r:apply (r:variable 'and) (list (r:bool #t) (r:bool #f)))))

(check-equal?
  (r:eval-exp
    (r:apply (r:variable '+)
      (list (r:number 1) (r:number 2) (r:number 3))))
  6)
(check-equal?
  (r:eval-exp
    (r:apply (r:variable 'and)
      (list (r:bool #t) (r:number 2) (r:number 3))))
  3)
