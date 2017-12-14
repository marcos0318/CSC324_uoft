#lang racket #| CSC 324 2017 Fall Assignment 3  |#

#| A Type Checker with some Inference |#

(provide type) ; Implement ‘type’.


#| The Language of Types.

 Base types: Boolean, Number, String, Void, and ⊥ [called “bottom”, \bot].
   • Represented by symbols with those names.

 Function type constructor: (<Type> ... → <Type>)
   • Represented by a list of zero or more types, followed by the symbol →, followed by a type. |#


#| The Syntactic Language of Expressions.


 <base-literal> - represented by a boolean, number, or string.

 <identifier> - represented by a symbol.

 For each of the following, a list with that structure, where ‘...’ means zero or more of the
  preceding component, parts in angle-brackets are from the corresponding category, and other
  non-list components appear literally as symbols. Except: <Type> is never ⊥.

 (λ ((<identifier> : <Type>) ...)
   <body-expression>
   ...
   <result-expression>)

 (let ([<identifier> <expression>] ...)
    <body-expression>
    ...
    <result-expression>)

 (rec (<function-identifier> (<identifier> : <Type>) ... : <result-Type>)
   <body-expression>
   ...
   <result-expression>)

 (<function-expression> <argument-expression> ...)

 (if <condition-expression>
     <consequent-expression>
     <alternative-expression>)

 (set! <identifier> <expression>) |#

#| The Type of an Expression.

 As with evaluation, the type of an expression is relative to a current environment that contains
  a mapping of variables in scope to their types.
 Also, if at any point a type during the check of an expression is ⊥, the type of the whole expression
  is ⊥, and the expression is said to “not type check”.

 <base-literal> : the corresponding base type

 <identifier> : the type of the most local identifier with that name in the environment

 (λ ((<identifier> : <Type>) ...)
   <body-expression>
   ...
   <result-expression>)

   • In the current environment with each <identifier> bound locally to its corresponding <Type>:
      if each <body-expression> type checks then the type is (<Type> ... → <Result-Type>),
      where <Result-Type> is the type of <result-expression>.

 (let ([<identifier> <expression>] ...)
    <body-expression>
    ...
    <result-expression>)

   • If <expression> ... type check as <Type> ..., then the type is the same as the type of:

       ((λ ((<identitifer> : <Type>) ...)
          <body-expression>
          ...
          <result-expression>)
        <expression> ...)

 (rec (<function-identifier> (<identifier> : <Type>) ... : <result-Type>)
   <body-expression>
   ...
   <result-expression>)

   • In the current environment with <function-identifier> bound locally to <result-Type>,
      the type is the same as the type of:
       (λ ((<identitifer> : <Type>) ...)
         <body-expression>
         ...
         <result-expression>)

 (<function-expression> <argument-expression> ...)

   • Type checks iff the type of <function-expression> is a function type and the types of
      <argument-expression> ... match the function type's argument types, in which case
      the type is the function type's result type.

 (if <condition-expression>
     <consequent-expression>
     <alternative-expression>)

   • Type checks iff the type of the condition is Boolean, and the consequent and alternative
      have the same type, in which case the type of the expression is the type of the consequent.

 (set! <identifier> <expression>)

   • Type checks iff the type of the most local identifier with that name in the environment
      is the type of <expression>, in which case the type is Void. |#

; type : Expression → Type
; You may choose whatever representation for the environment that you like.
(define (type expr [env '()])
  '⊥)

