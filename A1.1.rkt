#lang racket #| CSC 324 - 2017 Fall - Assignment 1 - Part 1 / 2 |#

#| Due Saturday October 28th at Noon.
   May be done with one other student in the course. |#

#| ★ Implement ‘eva’, an algebraic interpreter for an augmented Lambda Calculus.

 The syntactic language of Terms is the Lambda Calculus, augmented with numeric literals
  and a special constant ‘add1’ that when called with a numeric literal adds one to it.

   (λ (<identifier>) <term>)
     - represented by a list containing:
       • the symbol λ
       • a list containing an identifier
       • a term
   (add1 <a-term>)  done
     - represented by a list containing the symbol add1, and a term
   (<f-term> <a-term>)
     - represented by a list containing two terms
   <identifier>
     - represented by a symbol
   <literal>
     - represented by a number

 The semantics of function call is eager by-value, by algebraic substitution.

   (add1 <a-term>)
     1. Evaluate <a-term>, assume it produces a numeric literal.
     2. Add one to the value of <a-term>.

   (<f-term> <a-term>)
     1. Evaluate <f-term>, assume it produces a λ term: (λ (<id>) <body>) .
     2. Evaluate <a-term>, producing a value v.
     3. Substitute v into <body> by replacing <id> in <body> with v.
        Respect scope: if <body> contains a λ term whose parameter is also <id>,
         do not replace <id> anywhere in that λ term.
     4. Evaluate the transformed body.

   Any other term.
     The value of the term is itself. |#

(provide eva sub)

(module+ test
  (require rackunit)
  
  ; An example, that is already too complicated for the first test in test-driven development.
  (check-equal? (eva '(add1 0)) 1)
  (check-equal? (eva '((λ (x) (add1 x)) 0)) 1)
  (check-equal? (eva '((λ (x) x) 1)) 1)
  
  (check-equal? (sub 'x 0 '(λ (y) ((λ (x) x) x))) '(λ (y) ((λ (x) x) 0)))

  ; Your design and tests:
  (check-equal? (sub 'x 0 'x) 0)
  )


(define (eva term)
  (match term
    [`((λ (,id) ,body) ,a-term) (eva (sub id (eva a-term) body)) ]
    [`(add1 ,x) (add1 x)]
    [_ term]))


#| ★ Implement algebraic substitution.

 sub : symbol Term Term → Term
 Substitute value for id in term, respecting scope. |#

(define (sub id value term)
  (define (sub′ e) (sub id value e))
  (match term
    [`(λ (,id′) ,body) `(λ (,id′) ,(sub′ body))]
    [`(,f ,a) `(,f ,(sub′ a))]
    [id value]))
