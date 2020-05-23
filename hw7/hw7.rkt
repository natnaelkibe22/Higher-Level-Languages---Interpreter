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
(require "hw7-util.rkt")
(provide (all-defined-out))

(define/contract (env-put env var val)
  (-> handle? d:variable? d:value? eff-op?)
  (define result (d:void))
  (eff-op (lambda (h) (eff (environ-put h env var val) result)))
  )

(define/contract (env-push env var val)
  (-> handle? d:variable? d:value? eff-op?)
  (eff-op (lambda (h) (environ-push h env var val)))
  )

(define/contract (env-get env var)
  (-> handle? d:variable? eff-op?)
   (eff-op (lambda (h) (eff h (environ-get h env var))))
  )

(define/contract (d:eval-exp env exp)
  (-> handle? d:expression? eff-op?)
  (match exp 
            ; values 
             [(d:number n) (eff-pure (d:number n))]
             [(d:bool b) (eff-pure (d:bool b))]
             [(d:void) (eff-pure (d:void))]
             [(d:variable n)
                  (eff-op (lambda (mem) (eff mem (environ-get mem env exp))))
              ] 
             [(d:lambda xs t)
               (eff-op (lambda(mem) (eff mem (d:closure env exp))))
             ]
              ;code for if/else
             [(d:apply (d:apply (d:apply (d:variable 'if) 
                  (list e1)) (list e2)) (list ea))
                  (do 
                  tf <- (d:eval-exp env e1) 
                  (match tf
                       [(d:bool #f) (d:eval-exp env ea)] 
                       [_ (d:eval-exp env e2)]
                       ) 
                  )
                  ]
             [(d:apply ef (list ea))
             (do 
                Ef-closure <- (d:eval-exp env ef)
                (match Ef-closure 
                     [(d:closure Ef-closure-env (d:lambda (list arg) body))
                      (do 
                      va <- (d:eval-exp env ea) 
                      Ef <- (env-push Ef-closure-env arg va)
                      vb <- ((d:eval-term-impl) Ef body) 
                      (eff-pure vb) 
                      )
                      ]
                       [(d:builtin ef1) 
                       (do 
                       va <- (d:eval-exp env ea) 
                       (eff-pure (ef1 va)) 
                       )
                       ]
                )
             )
             ]
             [_ (eff-pure exp)]
             )
             
  )

(define/contract (d:eval-term env term)
  (-> handle? d:term? eff-op?)
    (match term
       [(d:define x body)
              (do 
              value <- ((d:eval-exp-impl) env body)
              (env-put env x value)
              )
      ]
      [(d:seq fst snd)
      (do
        v1 <- (d:eval-term env fst)
        (d:eval-term env snd)
      )
      ]
      [_ ((d:eval-exp-impl) env term)]
      )
)

;; Use this dynamic parameter in d:eval-term for improved testing (see Lecture 31)
(define d:eval-exp-impl (make-parameter d:eval-exp))
;; Use this dynamic parameter in d:eval-exp for improved testing (see Lecture 31)
(define d:eval-term-impl (make-parameter d:eval-term))

;; Parameter body *must* be a curried term already
(define/contract (break-lambda args body)
  (-> (listof d:variable?) d:term? d:lambda?)
    (match args
    [(list ) (d:lambda (list (d:variable '_)) body)]
    [(list x) (d:lambda (list x) body)]
    [(list x y ...) (d:lambda (list x) (break-lambda y body))])
    )

;; ef is a curried expression and args is a list of curried expressions
(define/contract (break-apply ef args)
  (-> d:expression? (listof d:expression?) d:expression?)
  (match args
   [(list ) (d:apply ef (list (d:void)))]
   [(list x) (d:apply ef (list x))]
   [(list x y ...)  (break-apply (d:apply ef (list x)) y)]
  )
  )

;; Use this dynamic parameter in d:curry for improved testing (see Lecture 31)
(define break-lambda-impl (make-parameter break-lambda))
;; Use this dynamic parameter in d:curry for improved testing (see Lecture 31)
(define break-apply-impl (make-parameter break-apply))

(define/contract (d:curry term)
  (-> d:term? d:term?)
    (match term 
       [(d:define x body) (d:define (d:curry x) (d:curry body))]
       [(d:lambda x body) ((break-lambda-impl) (d:curry x) (d:curry body))]
       [(d:seq fst snd) (d:seq (d:curry fst) (d:curry snd))]
       [(d:apply ef (list (d:apply f ea))) 
       ((break-apply-impl) (d:curry ef) (list ((break-apply-impl) (d:curry f) (d:curry ea))))
       ]
       [(d:apply ef ea) ((break-apply-impl) (d:curry ef) (d:curry ea))
       ]
       [_ term]
    )
)


