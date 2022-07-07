#lang eopl

;; Program    ::= Expression
;;
;; Expression ::= Number
;;
;;            ::= Identifier
;;
;;            ::= -(Expression, Expression)
;;
;;            ::= div(Expression, Expression)
;;
;;            ::= zero?(Expression)
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
;;            ::= if Expression then Expression else Expression
;;
;;            ::= let {Identifier = Expression}* in Expression
;;
;;            ::= proc ({Identifier}*(,)) Expression
;;
;;            ::= letrec Identifier (Identifier) = Expression in Expression
;;
;;            ::= (Expression {Expression}*)
;;
;;            ::= try Expression catch (Identifier) Expression
;;
;;            ::= raise Expression

(provide

 ;; AST
 program
 a-program

 expression expression?
 const-exp
 var-exp
 diff-exp
 div-exp
 zero?-exp
 cons-exp
 car-exp
 cdr-exp
 null?-exp
 emptylist-exp
 list-exp
 if-exp
 let-exp
 proc-exp
 letrec-exp
 call-exp
 try-exp
 raise-exp

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

    (expression ("div" "(" expression "," expression ")")
                div-exp)

    (expression ("zero?" "(" expression ")")
                zero?-exp)

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

    (expression ("if" expression "then" expression "else" expression)
                if-exp)

    (expression ("let" (arbno identifier "=" expression) "in" expression)
                let-exp)

    (expression ("proc" "(" (separated-list identifier ",") ")" expression)
                proc-exp)

    (expression ("letrec" identifier "(" identifier ")" "=" expression "in" expression)
                letrec-exp)

    (expression ("(" expression (arbno expression) ")")
                call-exp)

    (expression ("try" expression "catch" "(" identifier ")" expression)
                try-exp)

    (expression ("raise" expression)
                raise-exp)))

(sllgen:make-define-datatypes scanner-spec grammar)

(define parse
  (sllgen:make-string-parser scanner-spec grammar))
