#lang racket
(require rackunit)
(require "hw6-util.rkt")
(require "hw6.rkt")

;;;;;;;;;;;;;;;
;; Exercise 1

(check-equal? (frame-refs (parse-frame '[(x . 2) (y . 10) (z . 0)]))
  (set))
(check-equal? (frame-refs (parse-frame '[E10 (x . 2) (y . 10) (z . 0)]))
  (set (handle 10)))

(check-equal? (frame-refs (parse-frame '[(x . (closure E0 (lambda (x) x))) (y . 10) (z . (closure E1 (lambda (z) z)))]))
  (set (handle 0) (handle 1)))

(check-equal? (frame-refs (parse-frame '[E9 (x . (closure E0 (lambda (x) x))) (y . 10) (z . (closure E1 (lambda (z) z)))]))
  (set (handle 0) (handle 1) (handle 9)))
(check-equal? (frame-refs (parse-frame '[E9 (x . (closure E0 (lambda (x) x))) (y . 10) (z . (closure E1 (lambda (z) z))) (w . (closure E3 (lambda (z) z)))]))
  (set (handle 0) (handle 1) (handle 3) (handle 9)))


;;;;;;;;;;;;;;;
;; Exercise 2

(define m1
  (parse-mem
    '[(E0 . [(f . (closure E0 (lambda (x) (lambda (y) x))))])
      (E1 . [E0 (x . 2)])
      (E2 . [E0 (x . 10)])
      (E3 . [E0 (x . 5)])]))
(check-equal? (mem-mark frame-refs m1 (handle 0)) (set (handle 0)))
(check-equal? (mem-mark frame-refs m1 (handle 1)) (set (handle 0) (handle 1)))
(check-equal? (mem-mark frame-refs m1 (handle 2)) (set (handle 0) (handle 2)))
(check-equal? (mem-mark frame-refs m1 (handle 3)) (set (handle 0) (handle 3)))

(define m2
  (parse-mem
    '[(E0 . [(f . (closure E0 (lambda (x) (lambda (y) x))))])
      (E1 . [E0 (x . 2)])
      (E2 . [E1 (x . 10)])
      (E3 . [E3 (a . 5) (b . (closure E0 (lambda (x) (lambda (y) x)))) (c . (closure E1 (lambda (x) (lambda (y) x))))])
      (E4 . [E3 (x . (closure E0 (lambda (x) x))) (y . 10) (z . (closure E1 (lambda (z) z)))])]))
    
(check-equal? (mem-mark frame-refs m2 (handle 0)) (set (handle 0)))
(check-equal? (mem-mark frame-refs m2 (handle 1)) (set (handle 0) (handle 1)))
(check-equal? (mem-mark frame-refs m2 (handle 2)) (set (handle 0) (handle 1) (handle 2)))
(check-equal? (mem-mark frame-refs m2 (handle 3)) (set (handle 0) (handle 1) (handle 3)))
(check-equal? (mem-mark frame-refs m2 (handle 4)) (set (handle 0) (handle 1) (handle 3) (handle 4)))


;;;;;;;;;;;;;;;
;; Exercise 3

(check-equal? (mem-sweep m2 (set (handle 0)))
  (parse-mem
    '[(E0 . [(f . (closure E0 (lambda (x) (lambda (y) x))))])]))
(check-equal? (mem-sweep m2 (set (handle 0) (handle 1)))
  (parse-mem
    '[(E0 . [(f . (closure E0 (lambda (x) (lambda (y) x))))])
      (E1 . [E0 (x . 2)])]))
(check-equal? (mem-sweep m2 (set (handle 0) (handle 1)))
  (parse-mem
    '[(E0 . [(f . (closure E0 (lambda (x) (lambda (y) x))))])
      (E1 . [E0 (x . 2)])]))

;;;;;;;;;;;;;;;
;; Exercise 4

(define (pop)
  (eff-op
    (lambda (stack)
      (eff (rest stack) (first stack)))))
(define (push n)
  (eff-op
    (lambda (stack)
      (eff (cons n stack) (void)))))

(check-equal?
  (eff-run
    (mlist eff-bind eff-pure (list (eff-pure 1) (eff-pure 2) (eff-pure 3)))
    (list))
  (eff (list) (list 1 2 3)))

;;;;;;;;;;;;;;;
;; Exercise 5

;(check-equal? (eff-run (mapply eff-bind eff-pure * (pop) (pop)) (list 3 2)) (eff (list) 6))

