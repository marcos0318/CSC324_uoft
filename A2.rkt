#lang racket #| CSC324 2017 Fall Assignment 2 : Due Wednesday November 29 at 6PM. |#

#| The Maybe Monad.

 In this assignment you implement the functional API for computation that propagates false as failure,
  and use that to implement the associated Do notation. |#

(provide >> >>=
         ÷ √ ln
         E1 Do E2 E3)

(module+ test (require rackunit)
  
  ; Implement ‘>>’, called “then”, that takes two *expressions* and produces an expression that:
  ;   1. Evaluates the first expression.
  ;   2. If that produces false then that is the result.
  ;   3. Otherwise, evaluates the second expression and that is the result.
  
  (check-false (>> (not (zero? 0)) (/ 324 0)))
  (check-equal? (>> (not (zero? 324)) (/ 324 324))
                  1)
  (check-false (>> (number? "324") (>> (not (zero? "324")) (/ 324 "324"))))

  ; Implement functions ÷, √, and ln, that produce false if dividing by zero, taking the square root
  ;  of a negative number, or taking the logarithm of a non-positive number.
  ; Use ‘>>’ appropriately in the implementations.
  ; Implement ÷ curried: taking a number, and returning a unary function ready to divide a number.
  (check-false (√ -1))
  (check-equal? (√ 324) 18)
  (check-equal? (√ 0) 0)
  (check-false ((÷ 1) 0))
  (check-equal? ((÷ 324) 18) 18)
  (check-false (ln 0))
  (check-equal? (ln 324) (log 324))
  
  ; Implement *function* ‘>>=’, called “bind”, that takes two arguments and:
  ;  1. If the first argument is false then that is the result.
  ;  2. Otherwise, calls the second argument on the first.
  ; Use ‘>>’ appropriately in the implementation.
  (check-false (>>= -1 √))
  (check-false (>>= (number? "324") (÷ 324)))
  (check-equal? (>>= ((÷ 324) 18) (÷ 18)) 1)
  (check-equal? (>>= ((÷ 324) 36) (÷ 18)) 2)
  (check-false (>>= (>>= -1 √) ln))
  (check-equal? (>>= (>>= (>>= 324 √) (÷ 1)) ln)
                  (log (/ (sqrt 324)))))

(define-syntax-rule (>> e1 e2)
  (cond [(not e1) #false]
        [else e2]))


(define (÷ v1)
  (λ (v2) (>> (not (zero? v2)) (/ v1 v2))))

(define (√ v1)
  (>> (not (negative? v1)) (sqrt v1)))

(define (ln v1)
  (>> (positive? v1) (log v1)))

(define (>>= v f)
  (>> v (f v)))

; Consider this language of arithmetic expressions:
;   <numeric-literal>
;      - represented by a number
;   (√ <ae>)
;      - represented by a list with the symbol √  and an arithemtic expression
;   (ln <ae>)
;      - represented by a list with the symbol ln and an arithemtic expression
;   (<ae> ÷ <ae>)
;      - represented by a list with an arithmetic expression, symbol ÷, and arithemtic expression
  
; Implement function ‘E1’ to evaluate such expressions, producing false if any of the computations
;  are invalid according to the earlier restrictions for square root, logarithm, and division.
; Use pattern matching appropriately, along with ‘>>’ and ‘>>=’ for propagating false.
; In particular, do not use any other conditionals, nor boolean operations or literals.
; Use quasiquotation as appropriate for the patterns, but nothing else from match's pattern language
; [e.g. don't use ‘?’, nor #:when].
(define (E1 ae)
  (match ae
    [`(√ ,sub-ae) (>>= (E1 sub-ae) √) ]
    [`(,sub-ae1 ÷ ,sub-ae2) (>> (E1 sub-ae1) (>>= (E1 sub-ae2) (>>= (E1 sub-ae1) ÷)))]
    [`(ln ,sub-ae) (>>= (E1 sub-ae) ln)]
    [number number]))

;(E1 '(5 ÷ 0))
;(E1 '(5 ÷ 3))
;(E1 '(ln 100))
;(E1 '(ln 0))
;(E1 '(ln -1))
;(E1 '(√ 4))
;(E1 '(√ 0))
;(E1 '(√ -4))
;(E1 1)

;(E1 '((√ 4) ÷ (ln (10 ÷ 3))))
;(E1 '((√ -1) ÷ (ln (10 ÷ 3))))
;(E1 '((√ 4) ÷ (ln (-10 ÷ 0))))
;(E1 '((√ -4) ÷ (ln (-10 ÷ 0))))
;(E1 '(√ (10 ÷ 0)))


; Implement ‘Do’, using ‘>>’ and ‘>>=’ appropriately.
;
; It takes a sequence of clauses to be evaluated in order, short-circuiting to produce false if any
;  of the clauses produces false, producing the value of the last clause.
;
; Except for the last clause, a clause can be of the form
#;(identifier ← expression)
;  in which case its meaning is: evaluate the expression, and make the identifier refer to the
;  value in subsequent clauses.
;
; Don't use any local naming [local, let, match, define, etc] except for λ parameters:
;  recall that ‘let’ is just a notation for a particular lambda calculus “design pattern”.
  
(module+ test
  (check-equal? (Do 324)
                  324)
  (check-false (Do #false
                     (/ 1 0)))
  (check-false (Do (r1 ← (√ -1))
                     (r2 ← (ln (+ 1 r1)))
                     ((÷ r1) r2)))
  (check-false (Do (r1 ← (√ -1))
                     (r2 ← (ln (+ 1 r1)))
                     ((÷ r1) r2)))
  (check-equal? (Do ((÷ 4) 2)) 2)
  (check-equal? (Do (r1 ← (ln 2))
                     (r2 ← (√ 1))
                     ((÷ r1) r2)) (ln 2))

  (check-equal? (Do (r1 ← (√ 4))
                     (r2 ← (add1 (add1 r1)))
                     r2) 4)

  (check-equal? (Do (r1 ← (ln 2))
                     (r2 ← r1)
                     ((÷ r1) r2)) 1.0)

  )



(define-syntax Do
  (syntax-rules (←)
    [(Do (id ← ex)
        clause ...
        r)
     (>>= ex (λ(id)(Do
                    clause ...
                    r)))]
    [(Do #false clause ...) #false]
    [(Do r) r]))

; Implement ‘E2’, behaving the same way as ‘E1’, but using ‘Do’ notation instead of ‘>>’ and ‘>>=’.

(define (E2 ae)
  (match ae
    [`(√ ,sub-ae) (Do  (v ← (E2 sub-ae))
                       (√ v)) ]
    [`(,sub-ae1 ÷ ,sub-ae2) (Do (v1 ← (E2 sub-ae1))
                                (v2 ← (E2 sub-ae2))
                                ((÷ v1) v2))]
    [`(ln ,sub-ae) (Do (v ← (E2 sub-ae))
                       (ln v))]
    [number number]))


(E2 '(5 ÷ 0))
(E2 '(5 ÷ 3))
(E2 '(ln 100))
(E2 '(ln 0))
(E2 '(ln -1))
(E2 '(√ 4))
(E2 '(√ 0))
(E2 '(√ -4))
(E2 1)

(E2 '((√ 4) ÷ (ln (10 ÷ 3))))
(E2 '((√ -1) ÷ (ln (10 ÷ 3))))
(E2 '((√ 4) ÷ (ln (-10 ÷ 0))))
(E2 '((√ -4) ÷ (ln (-10 ÷ 0))))
(E2 '(√ (10 ÷ 0)))


; Implement ‘E3’, behaving the same way as ‘E2’, by expanding each use of ‘Do’ notation in ‘E2’,
;  and also replacing ‘E2’ with ‘E3’. The result will be similar to your ‘E1’, but likely a bit
;  less elegant.

(define (E3 ae)
  (match ae
    [`(√ ,sub-ae)
      (>>= (E3 sub-ae)
           (λ(id) (√ id)))]
    [`(,sub-ae1 ÷ ,sub-ae2)
     (>>= (E3 sub-ae1)
          (λ(v1)(>>= (E3 sub-ae2)
                     (λ(v2)((÷ v1) v2)))))]
    [`(ln ,sub-ae)
     (>>= (E3 sub-ae)
          (λ(id) (ln id)))]
    [number number]))


(E3 '(5 ÷ 0))
(E3 '(5 ÷ 3))
(E3 '(ln 100))
(E3 '(ln 0))
(E3 '(ln -1))
(E3 '(√ 4))
(E3 '(√ 0))
(E3 '(√ -4))
(E3 1)

(E3 '((√ 4) ÷ (ln (10 ÷ 3))))
(E3 '((√ -1) ÷ (ln (10 ÷ 3))))
(E3 '((√ 4) ÷ (ln (-10 ÷ 0))))
(E3 '((√ -4) ÷ (ln (-10 ÷ 0))))
(E3 '(√ (10 ÷ 0)))