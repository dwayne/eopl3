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
;;            ::= zero?(Expression)
;;
;;            ::= equal?(Expression, Expression)
;;
;;            ::= greater?(Expression, Expression)
;;
;;            ::= less?(Expression, Expression)
;;
;;            ::= cons(Expression, Expression)
;;
;;            ::= car(Expression)
;;
;;            ::= cdr(Expression)
;;
;;            ::= null?(Expression)
;;
;;            ::= emptylist
;;
;;            ::= list({Expression}*(,))
;;
;;            ::= unpack {Identifier}* = Expression in Expression
;;
;;            ::= if Expression then Expression else Expression
;;
;;            ::= cond {Expression ==> Expression}* end
;;
;;            ::= let {Identifier = Expression}* in Expression
;;
;;            ::= let* {Identifier = Expression}* in Expression
;;
;;            ::= proc (Identifier) Expression
;;
;;            ::= (Expression Expression)

(provide

 ;; AST
 program
 a-program

 expression expression?
 const-exp
 var-exp
 add-exp
 diff-exp
 mul-exp
 div-exp
 minus-exp
 zero?-exp
 equal?-exp
 greater?-exp
 less?-exp
 cons-exp
 car-exp
 cdr-exp
 null?-exp
 emptylist-exp
 list-exp
 unpack-exp
 if-exp
 cond-exp
 let-exp
 let*-exp
 proc-exp
 call-exp

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

    (expression ("zero?" "(" expression ")")
                zero?-exp)

    (expression ("equal?" "(" expression "," expression ")")
                equal?-exp)

    (expression ("greater?" "(" expression "," expression ")")
                greater?-exp)

    (expression ("less?" "(" expression "," expression ")")
                less?-exp)

    (expression ("cons" "(" expression "," expression ")")
                cons-exp)

    (expression ("car" "(" expression ")")
                car-exp)

    (expression ("cdr" "(" expression ")")
                cdr-exp)

    (expression ("null?" "(" expression ")")
                null?-exp)

    (expression ("emptylist")
                emptylist-exp)

    (expression ("list" "(" (separated-list expression ",") ")")
                list-exp)

    (expression ("unpack" (arbno identifier) "=" expression "in" expression)
                unpack-exp)

    (expression ("if" expression "then" expression "else" expression)
                if-exp)

    (expression ("cond" (arbno expression "==>" expression) "end")
                cond-exp)

    (expression ("let" (arbno identifier "=" expression) "in" expression)
                let-exp)

    (expression ("let*" (arbno identifier "=" expression) "in" expression)
                let*-exp)

    (expression ("proc" "(" identifier ")" expression)
                proc-exp)

    (expression ("(" expression expression ")")
                call-exp)))

(sllgen:make-define-datatypes scanner-spec grammar)

(define parse
  (sllgen:make-string-parser scanner-spec grammar))
