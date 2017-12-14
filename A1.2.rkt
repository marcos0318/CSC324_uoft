#lang racket #| CSC 324 - 2017 Fall - Assignment 1 - Part 2 / 2 |#

#| Due Saturday October 28th at Noon.
   May be done with one other student in the course. |#

#| ★ Implement ‘Eva’, a memory model based interpreter for an augmented Lambda Calculus.

 The syntactic language of Terms is the same as in Part 1, except for (add1 <term>).

 A run-time value is one of:
   • a number
   • a closure: (closure: (<λ-address> (<id>) <body>) <environment>)
     - a list containing the symbol closure:, a λ term with the symbol λ replaced by a unique symbol,
        and an environment
   • an address of a closure

 An environment is a list of two-element lists: ((<id> <value>) ...).
   Earlier <id>s are more local than later ones, i.e. they shadow later ones.
   The <value>s are numbers or addresses.

 The model maintains a table associating addresses to closures.
 
 The semantics of function call is still eager by-value, with arguments passed by augmenting
  an environment.

   (<f> <a>)
     1. Evaluate <f> in the current environment, assume it produces an address of a closure c.
     2. Evaluate <a> in the current environment.
     3. Produce the value of the body of the λ term in c, evaluated in the environment of c
         augmented with a local variable associating the λ term's parameter with the value of <a>.

   (λ (<id>) <body>)
     1. Create a new closure with a new address, containing this λ term and current environment.
     2. Add the association of the address with the closure to the table of closures.
     3. Produce the address of the closure.

   <id>
     1. Produce the most local value of <id> from the current environment.

   <number>
     1. Produce the number. |#

(provide Eva)

#| Support library. |#

(define (lookup key table)
  (second (first (filter (λ (binding) (equal? (first binding) key)) table))))

(define (addresses)
  (define n -1)
  (λ () (local-require (only-in racket/syntax format-symbol))
    (set! n (add1 n))
    (format-symbol "λ~a" n)))

(module+ test
  (require rackunit)
  
  (check-equal? (lookup 'a '((b c) (a d) (a e))) 'd)

  (define generate-address (addresses))
  (check-equal? (generate-address) 'λ0)
  (check-equal? (generate-address) 'λ1))


#| Design and testing for Eva. |#

(module+ test
  
  ; An example, that is much too complicated for the first test in test-driven development.
  (check-equal? (Eva '((λ (x) (λ (y) x)) (λ (y) 324)))
                '(λ2 ; Result value.
                  ; Table associating addresses to closures.
                  ((λ2 (closure: (λ (y) x) ((x λ1))))
                   (λ1 (closure: (λ (y) 324) ()))
                   (λ0 (closure: (λ (x) (λ (y) x)) ())))))

  ; Your design and tests:
  (check-equal? (Eva '(λ (y) 324))
                '(λ0
                  ((λ0 (closure: (λ (y) 324) ())))))
  
  (check-equal? (Eva 324)
                '(324 ()))
  (check-equal? (Eva '((λ (x) x) 1))
                '(1
                  ((λ0 (closure: (λ (x) x) ())))))
)

#| Eva takes a term and produces:
     1. Its value, which is an address or number.
     2. The table associating addresses to closures. |#

(define (Eva term)

  (define generate-address (addresses))
  (define (add_env f_addr pairs table)
    (map (λ (item)
           (if (equal? (first item) f_addr)
               (list (first item) (list (first (second item)) (second (second item)) (list* pairs (third (second item)))))
               item))
         table))
  (define closures '())
  
  (define (eva term env)
    (match term
      
      [`(,f ,a)
       (define c (lookup (eva f env) closures))
       (eva (third (second c)) (list* `(,(first (second (second c))) ,(eva a env)) (third c)))
      ]
      
      [`(λ (,id) ,body)
       (define _id 0)
       (set! _id (generate-address))
       (set! closures (list* (list _id `(closure: ,term ,env)) closures))
       _id
      ]
      [id_number
       (define result 0)
       (and (number? id_number) (set! result id_number))
       (and (not (number? id_number)) (set! result (lookup id_number env)))
       result
       ]))
  
  (list (eva term '()) ; The value of term, in an empty initial environment.
        ; The table associating addresses to closures.
        closures))

