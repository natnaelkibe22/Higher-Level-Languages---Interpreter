#|
    ===> PLEASE DO NOT DISTRIBUTE THE SOLUTIONS PUBLICLY <===

   We ask that solutions be distributed only locally -- on paper, on a
   password-protected webpage, etc.

   Students are required to adhere to the University Policy on Academic
   Standards and Cheating, to the University Statement on Plagiarism and the
   Documentation of Written Work, and to the Code of Student Conduct as
   delineated in the catalog of Undergraduate Programs. The Code is available
   online at: http://www.umb.edu/life_on_campus/policies/code/
   Name: Natnael Kibe     

|#
;; PLEASE DO NOT CHANGE THE FOLLOWING LINES
#lang errortrace racket
(provide (all-defined-out))
(require "hw4-util.rkt")
;(require "hw2.rkt")
;; END OF REQUIRES

;; Utility functions
(define (s:apply-arg1 app)
  (first (s:apply-args app)))
(define (s:lambda-param1 lam)
  (first (s:lambda-params lam)))
(define (s:lambda-body1 lam)
  (first (s:lambda-body lam)))
;; Utility functions
(define (e:apply-arg1 app)
  (first (e:apply-args app)))
(define (e:lambda-param1 lam)
  (first (e:lambda-params lam)))
(define (e:lambda-body1 lam)
  (first (e:lambda-body lam)))

;; Exercise 1
(define (s:subst exp var val) 
  (cond[(s:expression? exp) 
       (cond[(s:variable? exp) 
            (cond[(equal? exp var) val]
            [else exp]
            )]   
         [(s:value? exp) 
             (cond[(s:number? exp) exp]
             [else 
                (cond[(not (equal? (s:lambda-param1 exp) (s:lambda-body1 exp))) 
                   (s:lambda (list (s:lambda-param1 exp)) (list (s:subst (s:lambda-body1 exp) var val)))
                ]
                [else (s:lambda (list (s:lambda-param1 exp)) (list (s:lambda-body1 exp)))])
             ]
        )]
        [else (s:apply (s:subst (s:apply-func exp) var val) (list (s:subst (s:apply-arg1 exp) var val)))])
     ]
     [else 
     (error "Unknown expression:" exp)
     ]
     )
)
;; Exercise 2
(define (s:eval subst exp) 
    (cond[(s:expression? exp) 
            (cond[(s:value? exp) exp]
            [(s:variable? exp) (error "Unknown argument:" exp)]
            [(s:apply? exp) 
                  (s:eval subst (s:subst (s:lambda-body1 (s:eval subst (s:apply-func exp)))
                        (s:lambda-param1 (s:eval subst (s:apply-func exp))) (s:eval subst (s:apply-arg1 exp))))
            ])
     ]
     [else 
     (error "Unknown expression:" exp)
     ]
     )
)

;; Exercise 3
                ;unfolding e:apply
                ;Eb = closure-env(e:eval env ef)
                ;x => (lambda-param1 (closure-decl (e:eval env ef)))
                ;env1 -> Eb[x-> va] = (hash-set Eb x va)
                ;va-> (e:eval env (e:apply-arg1 exp))
                ;eb -> (lembda-body1 (closure-decl (e:eval env ef)))
                ;ef => (e:apply-func exp)
(define (e:eval env exp) 
     (cond[(e:expression? exp) 
            (define h (hash))
            (cond  
             [(e:value? exp) exp]
             [(and (e:variable? exp) (hash? env) (hash-has-key? env exp))
                (hash-ref env exp)] 
             [(and (e:lambda? exp) (hash? env))
               (e:closure env exp)
             ]
             [(e:apply? exp)
                (define ea (e:apply-arg1 exp))
                (define ef (e:apply-func exp))
                (define va (e:eval env ea))   
                (define Eb (e:closure-env (e:eval env ef)))
                (define x (e:lambda-param1 (e:closure-decl (e:eval env ef))))
                (define eb (e:lambda-body1 (e:closure-decl (e:eval env ef))))
                (define env1 (hash-set Eb x va))
                (e:eval env1 eb)
             ]
             )]
     [else 
     (error "Unknown expression:" exp)
     ]
     )
)

;; Exercise 4 (Manually graded)
#|
Lambda-Racket with environment is needed to save programs that have a higher time-complexity due to many variables and the 
program has to search those variables recursively in order to replace them. So the main reason we need a lambda-racket 
that have an environment book keeping place for variable-binding is to save time by sacrificing some memory space. 
In my opinion Lambda-racket with environment seems to have no weakness but for instance, let say we want to replace or connect
a procedure parameter with its function then I will say using lambda-racket without environment is efficient or straight forward.
Otherthan that, I don't think lambda-racket without book keeping method for variables and lambda is better than lambda-racket with 
environment.
|#

;; Exercise 5 (Manually graded)
#|
I will say a formally defined or like a pseudocode type of language is more intuitive and more understanding for even a child to 
read and it makes it easier for understanding very complex algorithms in software engineering. For instance, let say someone tried
to deconstruct how DFS (Depth First Search) is different from BFS (Breadth First Search) using mnemonic and easy formal language 
to understand for anyone then that will make the algorithm of DFS and BFS very very intuitive like navigating a gps kind of easy problem.
But sometimes understanding or trying to understand vague problems using just code is very problematic since we might run into syntax 
errors which would affect our way of thinking about the algorithm or our thinking of the problem or our way of thinking to solve a software
engineering problem. So for any kind of software engineering problem, it is very imperative to deconstruct the problem into little bits
using formal language or formal specified language for any software engineering or it could even apply to learning any skill. Formal specification
is very important for learning and problem solving purpose of software engineering. 
|#
