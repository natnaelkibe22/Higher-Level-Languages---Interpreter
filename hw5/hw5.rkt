#|
    ===> PLEASE DO NOT DISTRIBUTE THE SOLUTIONS PUBLICLY <===

   We ask that solutions be distributed only locally -- on paper, on a
   password-protected webpage, etc.

   Students are required to adhere to the University Policy on Academic
   Standards and Cheating, to the University Statement on Plagiarism and the
   Documentation of Written Work, and to the Code of Student Conduct as
   delineated in the catalog of Undergraduate Programs. The Code is available
   online at: http://www.umb.edu/life_on_campus/policies/code/

|#
;; PLEASE DO NOT CHANGE THE FOLLOWING LINES
#lang errortrace racket
(require "hw5-util.rkt")
(require rackunit)
(provide d:eval-exp d:eval-term)
(define (d:apply-arg1 app)
  (first (d:apply-args app)))
(define (d:lambda-param1 lam)
  (first (d:lambda-params lam)))
;; END OF REQUIRES

;; Exercise 1
(define/contract (d:eval-exp mem env exp)
  (-> mem? handle? d:expression? eff?)
        (cond  
             [ (d:variable? exp)
                  (eff mem (environ-get mem env exp))
              ] 
             [(d:lambda? exp)
               (eff mem (d:closure env exp))
             ]
             [(d:apply? exp)
                (define ef (d:apply-func exp))
                ;(define va (d:eval-exp mem env (d:apply-arg1 exp)))   
                (define Ef-closure (d:eval-exp mem env ef))
                (define ea (d:eval-exp (eff-state Ef-closure) env (d:apply-arg1 exp)))
                (define Ef (d:closure-env (eff-result Ef-closure)))
                (define x (d:lambda-param1 (d:closure-decl (eff-result Ef-closure))))
                (define tb (d:lambda-body (d:closure-decl (eff-result Ef-closure))))
                (define Ef+new-mem (environ-push (eff-state ea) Ef x (eff-result ea)))
                (define vb (d:eval-term (eff-state Ef+new-mem) (eff-result Ef+new-mem) tb))
                (eff (eff-state vb) (eff-result vb))
             ]
             [else (eff mem exp)]
             )
     
  )
;; Exercise 2
(define/contract (d:eval-term mem env term)
  (-> mem? handle? d:term? eff?)
      (cond
      [(d:define? term)
              ; v ->  e-> lambda-body1 -> evaluates to a value
              ;
              (define v (d:eval-term mem env (d:define-body term)))
              (define new-mem (environ-put (eff-state v) env (d:define-var term) (eff-result v)))
              (d:eval-term new-mem env (d:void))
      ]
      [(d:seq? term)
          (define v1 (d:eval-term mem env (d:seq-fst term)))
          (define v2 (d:eval-term (eff-state v1) env (d:seq-snd term)))
          v2
      ]
      [else (d:eval-exp mem env term)]
      )
)
 

;; Exercise 3 (Manually graded)
#|
  In lambda D racket, we can bind variables and function closures which can allow us t ohave the ability to define higher order functionality in 
  this programming language. Lambda D racket can also allow us to write free variables or variables with no bindings. What makes lambda D racket 
  compicating from regluar racket is that the use of mutable environments and heap memory cell, making it harder. In my opinion, lambda D racket
  need mutable environments and other constraints in order to function fully which makes it easier to debug the code actually because we have many information
  about the environment and state of a variable and functions. But in regular racket, it is very tougher to debug teh code or to evaluate it because
  if we don't know which varible is used or which free variable is bound to which lambda expression then it will be much much harder. It is even 
  very confusing when we use the same variables for many instances which makes it confusing. 
|#