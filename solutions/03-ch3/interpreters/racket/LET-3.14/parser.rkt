#lang eopl

;; Program    ::= Expression
;;
;; Expression ::= Number
;;
;;            ::= Identifier
;;
;;            ::= add(Expression, Expression)
;;
;;            ::= -(Expression, Expression)
;;
;;            ::= mul(Expression, Expression)
;;
;;            ::= div(Expression, Expression)
;;
;;            ::= minus(Expression)
;;
;;            ::= if Bool-exp then Expression else Expression
;;
;;            ::= let Identifier = Expression in Expression
;;
;; Bool-exp   ::= zero?(Expression)
;;
;;            ::= equal?(Expression, Expression)
;;
;;            ::= greater?(Expression, Expression)
;;
;;            ::= less?(Expression, Expression)

(provide

 ;; AST
 program
 a-program

 expression
 const-exp
 var-exp
 add-exp
 diff-exp
 mul-exp
 div-exp
 minus-exp
 if-exp
 let-exp

 bool-exp
 zero?-exp
 equal?-exp
 greater?-exp
 less?-exp

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

    (expression ("add" "(" expression "," expression ")")
                add-exp)

    (expression ("-" "(" expression "," expression ")")
                diff-exp)

    (expression ("mul" "(" expression "," expression ")")
                mul-exp)

    (expression ("div" "(" expression "," expression ")")
                div-exp)

    (expression ("minus" "(" expression ")")
                minus-exp)

    (expression ("if" bool-exp "then" expression "else" expression)
                if-exp)

    (expression ("let" identifier "=" expression "in" expression)
                let-exp)

    (bool-exp ("zero?" "(" expression ")")
              zero?-exp)

    (bool-exp ("equal?" "(" expression "," expression ")")
              equal?-exp)

    (bool-exp ("greater?" "(" expression "," expression ")")
              greater?-exp)

    (bool-exp ("less?" "(" expression "," expression ")")
              less?-exp)))

(sllgen:make-define-datatypes scanner-spec grammar)

(define parse
  (sllgen:make-string-parser scanner-spec grammar))
