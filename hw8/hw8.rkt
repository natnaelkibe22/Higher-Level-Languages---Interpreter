#lang errortrace racket
(require "hw8-util.rkt")

(provide (all-defined-out))

;; Utility function that converts a variable into a string
;; Useful when translating from SimpleJS into LambdaJS
(define (mk-field x)
  (match x 
  [(s:variable x) (k:string (symbol->string x))]
  )
  )

;; Utility function that allocates a j:object.
;; (mk-object) allocates an empty object
;; (mk-object (cons "foo" (k:number 1)) (cons "bar" (j:variable 'x)))
;;  allocates an object with one field "foo" and one field "bar"
(define/contract (mk-object . args)
  (-> (cons/c string? j:expression?) ... j:alloc?)
  (define (on-elem pair)
    (cons (k:string (car pair)) (cdr pair)))
  (j:alloc (j:object (make-immutable-hash (map on-elem args)))))

;;;;;;;;;;;;;;;
;; Exercise 1

(define/contract (translate exp)
  (-> s:expression? j:expression?)
  (match exp
    [(? k:const? k) k]
    [(s:variable x) (j:variable x)]
    [(s:let (s:variable x) s1 s2)
     (j:let (j:variable x) (translate s1) (translate s2))]
    [(s:apply f ea) (j:apply (translate f) (map translate ea))]
    [(s:load obj field) (j:get (j:deref (translate obj)) (mk-field field))]
    [(s:invoke obj meth args) 
      (mk-let (j:get (j:deref (translate obj)) (mk-field meth)) 
    (lambda (m) (mk-let (j:get (j:deref m) (k:string "$code")) 
    (lambda (f) (j:apply f (cons (translate obj) (map translate args)))))))
    ]
    [(s:assign obj field arg)
      (mk-let (translate arg)
      (lambda (data) (mk-let (j:deref (translate obj)) 
      (lambda (o) (j:seq (j:assign (translate obj) (j:set o (mk-field field) data)) data)))))
      ]
    [(s:function xs t) 
    (mk-object
      (cons "$code"
        (j:lambda (cons (j:variable 'this) (map translate xs)) (translate t)))
        (cons "prototype" (mk-object)))
    ]
    [(s:new constr args) 
       (mk-let (j:deref (translate constr))
       (lambda (ctor) (mk-let (mk-object (cons "$proto" (j:get ctor (k:string "prototype")))) 
       (lambda (obj) (mk-let 
        (j:get ctor (k:string "$code"))
        (lambda(f) (j:seq (j:apply f (cons obj (map translate args))) obj)))))))
    ]  
    )
    )
    