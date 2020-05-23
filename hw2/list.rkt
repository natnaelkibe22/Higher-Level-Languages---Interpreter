#lang racket
(require rackunit)
(define (cell-get c) (c (list)))
(define (cell-set c x) (c (list x)))


(define w1 (list 10))
(check-equal? 10 (cell-get w1))
(define w2 (cell-set w1 20))
(check-equal? 20 (cell-get w2))






(if
    (and (list? l)(equal? #f (null? v))(equal? #f (empty? l)))
      (if  (= (length l) 1)
                l
             (cons (first l)
               (cons v (intersperse (rest l) v))
             )
          )
       l
  )
