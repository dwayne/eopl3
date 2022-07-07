#lang eopl

;; Program    ::= Expression
;;
;; Expression ::= Number
;;
;;            ::= Identifier
;;
;;            ::= -(Expression, Expression)
;;
;;            ::= minus(Expression)
;;
;;            ::= zero?(Expression)
;;
;;            ::= if Expression then Expression else Expression
;;
;;            ::= let Identifier = Expression in Expression

(provide

 ;; AST
 program
 a-program

 expression
 const-exp
 var-exp
 diff-exp
 minus-exp
 zero?-exp
 if-exp
 let-exp

 ;; Parser
 parse)

(define scanner-spec
  '((number (digit (arbno digit)) number)
    (identifier (letter (arbno letter)) symbol)
    (ws ((arbno whitespace)) skip)))

(define grammar
  '((program (expression)
             a-program)

    (expression (number)
                const-exp)

    (expression (identifier)
                var-exp)

    (expression ("-" "(" expression "," expression ")")
                diff-exp)

    (expression ("minus" "(" expression ")")
                minus-exp)

    (expression ("zero?" "(" expression ")")
                zero?-exp)

    (expression ("if" expression "then" expression "else" expression)
                if-exp)

    (expression ("let" identifier "=" expression "in" expression)
                let-exp)))

(sllgen:make-define-datatypes scanner-spec grammar)

(define parse
  (sllgen:make-string-parser scanner-spec grammar))
