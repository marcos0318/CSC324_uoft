#lang racket #| CSC 324 2017 Fall Assignment 3  |#

#| A Type Checker with some Inference |#

(provide type) ; Implement ‘type’.

(define (check_env id env)
    (if (empty? (filter (λ (variable-pair) (equal? (first variable-pair) id)) env))
        '⊥
        (second (first (filter (λ (variable-pair) (equal? (first variable-pair) id)) env)))))

;take ((x : Boolean) (y : Number)) and env to (list* (x Boolean) (y Number) env)
(define (add_env arg_types env)
  (append (map (λ(trip) (remove ': trip)) arg_types) env))

(module+ test
  (require rackunit)
  (define env '((b String) (c Number) (a String)))
  (check-equal? (check_env 'a env) 'String )
  (check-equal? (check_env 'b env) 'String )
  (check-equal? (check_env 'c env) 'Number )
  (check-equal? (check_env 'd env) '⊥ )
  (check-equal? (add_env '((x : Boolean) (y : Number)) env) '((x Boolean) (y Number) (b String) (c Number) (a String))))


(define (type expr [env '()] ) 
  (match expr
    ;λ expressions
    [`(λ (,id-with-type ...) ,body-expr ... ,result-expr)
     (cond [(equal? (type result-expr (add_env id-with-type env)) '⊥)  '⊥];The result-expr is ⊥
           [(not (empty? (filter (λ (a-type) (equal? a-type '⊥)) (map (λ(x) (type x (add_env id-with-type env))) body-expr))))'⊥];Check body
           [else `( ,@(map third id-with-type)  →  ,(type result-expr (add_env id-with-type env)))])];If all good
    
    ;function calls, for λ
    [`((λ (,id-with-type ...) ,body-expr ... ,result-expr)
       ,arg-expr ...) (cond [(not (equal? (map (λ(x)(type x env)) arg-expr) (map third id-with-type))) '⊥]
                            ;check the arg-expr first, if not match, gg
                            [(equal? (type (first expr)) '⊥)  '⊥]
                            ;add the  id-type to the environment and evaluate each body expr and result expr
                            [else (type result-expr (add_env id-with-type env))])]
    
    ;let expressions
    [`(let (,lists ...) ,body-expr ... ,result-expr)
     (cond [(not (empty? (filter (λ (a-type) (equal? a-type '⊥)) (map (λ(item)(type (second item) env)) lists))))'⊥]
           [else (type `((λ ,(map (λ (item) (list (first item) ': (type (second item) env))) lists) (unquote-splicing body-expr) ,result-expr) ,@(map (λ (item)(second item)) lists)) env)])]

    ;if expression
    [`(if ,condition ,then ,else) (cond [(not (equal? (type condition env) 'Boolean)) '⊥]
                                   [(equal? (type then env) '⊥) '⊥]
                                   [(equal? (type else env) '⊥) '⊥]
                                   [(equal? (type then env) (type else env)) (type else env)]
                                   [else '⊥])]
    
    ;set! expressionss
    [`(set! ,id ,expr) (cond [(equal? (check_env id env) '⊥) '⊥]
                             [(equal?  (check_env id env) (type expr)) 'Void]
                             [else '⊥])]
    
    ;rec expresssion
    [`(rec (,f-id ,id-lists ... : ,result-Type)
        ,body-expr ...
        ,result-expr)
     (type `(λ ,id-lists ,@body-expr ,result-expr) (list* `(,f-id ,result-Type) env))]

    ; function call in environment
    [`(,f-id ,arg-expr ...) (cond [(equal? (check_env f-id env) '⊥) '⊥]
                                  [(not (empty? (filter (λ (a-type) (equal? a-type '⊥)) (map (λ(arg)(type arg env)) arg-expr)))) '⊥]
                                  [else `,(check_env f-id env)])]
    
    ;literals and symbols
    [base (cond
            [(string? expr) 'String]
            [(number? expr) 'Number]
            [(boolean? expr) 'Boolean]
            [(symbol? expr) (check_env expr env)];find the result from current env
            [else '⊥])]
    ))


(module+ test
  (require rackunit)
  ;basic
  (check-equal? (type '(λ ((x : Boolean) (y : Number)) #true)) '(Boolean Number → Boolean))
  ;with valid body
  (check-equal? (type '(λ ((x : Boolean) (y : Number))  233 "haha" "hello")) '(Boolean Number → String))
  ;with invalid body
  (check-equal? (type '(λ ((x : Boolean) (y : Number))  (add 1) "haha" "hello")) '⊥)
  ;with invalid result
  (check-equal? (type '(λ ((x : Boolean) (y : Number))  233 "haha" (add1 1))) '⊥)
  ;with env
  (check-equal? (type '(λ ((x : Boolean) (y : Number))  233 "haha" c) '((c String))) '(Boolean Number → String))
  ;with things not in env
  (check-equal? (type '(λ ((x : Boolean) (y : Number))  w "haha" c) '((c String))) '⊥)
  ;with the usage of argument in the body/result expressions
  (check-equal? (type '(λ ((x : Boolean) (y : Number))  233 "haha" x) '((c String))) '(Boolean Number → Boolean))

  (check-equal? (type #t) 'Boolean)
  
  ; basic lambda
  (check-equal? (type '((λ ((x : Boolean) (y : Number)) 233) 2 3)) '⊥)
  ; basic lambda
  (check-equal? (type '((λ ((x : Boolean) (y : Number)) 233) #t 3)) 'Number)
  ; with argument
  (check-equal? (type '((λ ((x : Boolean) (y : Number)) x) #t 3)) 'Boolean)

  (check-equal? (type '(if #true "abc" #true)) '⊥)
  (check-equal? (type '(if #true 324 325)) 'Number)
  (check-equal? (type '(if #true "adc" "efs")) 'String)
  (check-equal? (type '(if "abc" #true #false)) '⊥)
  (check-equal? (type '(if #t 2 3)) 'Number)
  (check-equal? (type '(if #t 2 "3")) '⊥)
  (check-equal? (type '(if 3 2 3)) '⊥)

  (check-equal? (type '(let ([x 3] [y 4]) 3)) 'Number)
  (check-equal? (type '(let ([x "a"] [y "b"]) 3)) 'Number)
  (check-equal? (type '(let ([x "a"] [y 4]) "str")) 'String)
  (check-equal? (type '(let ([x 3] [y 4]) "str")) 'String)
  (check-equal? (type '(let ([x 3] [y 4]) ⊥)) '⊥)
  (check-equal? (type '(let ([x 3] [y 4]) x)) 'Number)

  (check-equal? (type '(set! a 1) '((a Number))) 'Void)
  (check-equal? (type '(set! a "a") '((a Number))) '⊥)
  (check-equal? (type '(set! a "a") '((a String))) 'Void)
  (check-equal? (type '(set! b #true) '((b Boolean))) 'Void)
  
  (check-equal? (type `(rec (f (x : String) (y : Number) : Boolean) (f "str" 6))) `(String Number → Boolean))
  (check-equal? (type `(rec (f (x : String) (y : Number) : Boolean) (if #f 1 "s")(f "str" 6))) `⊥))
