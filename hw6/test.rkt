#lang racket
(require rackunit)
(require "hw6-util.rkt")
(require "hw6.rkt")

(frame-values (parse-frame '[(x . 2) (y . 10) (z . 0)]))
(frame-parent (parse-frame '[(x . 2) (y . 10) (z . 0)]))
(first (frame-values (parse-frame '[(x . 2) (y . 10) (z . 0)])))
(frame-values (parse-frame '[E10 (x . 2) (y . 10) (z . 0)]))
(frame-parent (parse-frame '[E10 (x . 2) (y . 10) (z . 0)]))
(frame-values (parse-frame '[(x . (closure E0 (lambda (x) x))) (y . 10) (z . (closure E1 (lambda (z) z)))]))
(frame-parent (parse-frame '[(x . (closure E0 (lambda (x) x))) (y . 10) (z . (closure E1 (lambda (z) z)))]))
 (frame-values (parse-frame '[E9 (x . (closure E0 (lambda (x) x))) (y . 10) (z . (closure E1 (lambda (z) z)))]))
 (frame-parent (parse-frame '[E9 (x . (closure E0 (lambda (x) x))) (y . 10) (z . (closure E1 (lambda (z) z)))]))
(second (frame-values (parse-frame '[E9 (x . (closure E0 (lambda (x) x))) (y . 10) (z . (closure E1 (lambda (z) z)))])))
(d:closure-env (second (frame-values (parse-frame '[E9 (x . (closure E0 (lambda (x) x))) (y . 10) (z . (closure E1 (lambda (z) z)))]))))
(d:closure-env (first (rest (rest (frame-values (parse-frame '[E9 (x . (closure E0 (lambda (x) x))) (y . 10) (z . (closure E1 (lambda (z) z)))]))))))


(eff-run (eff-bind (eff-pure 1) (eff-pure 2)) (list))

;(eff-run (eff-pure (list 1 2 3)) (list))


#|
 (cond
     [(frame-parent frm) (set-add (foldl proc (set) (frame-values frm)) (frame-parent frm))] 
    [else (foldl proc (set) (frame-values frm))]))

     (define wrp (lambda (var val accum) 
     (cond [(d:closure? val) (set-add accum (d:closure-env val))] 
     [else accum])))
  (cond
    [(frame-parent frm) (set-add (frame-fold wrp (set) frm) (frame-parent frm))] 
    [else (frame-fold wrp  (set) frm)]))

    (list (d:number 10) (d:closure (handle 1) (d:lambda (list (d:variable 'z)) (d:variable 'z))) (d:closure (handle 0) (d:lambda (list (d:variable 'x)) (d:variable 'x))))

|#

