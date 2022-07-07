#lang eopl

;; Program    ::= Expression
;;
;; Expression ::= Number
;;
;;            ::= Identifier
;;
;;            ::= -(Expression, Expression)
;;
;;            ::= list({Expression}*(,))
;;
;;            ::= zero?(Expression)
;;
;;            ::= if Expression then Expression else Expression
;;
;;            ::= let Identifier = Expression in Expression
;;
;;            ::= unpack {Identifier}* = Expression in Expression
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
 diff-exp
 list-exp
 zero?-exp
 if-exp
 let-exp
 unpack-exp
 proc-exp
 call-exp

 nameless-var-exp
 nameless-let-exp
 nameless-unpack-exp
 nameless-proc-exp

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

    (expression ("list" "(" (separated-list expression ",") ")")
                list-exp)

    (expression ("zero?" "(" expression ")")
                zero?-exp)

    (expression ("if" expression "then" expression "else" expression)
                if-exp)

    (expression ("let" identifier "=" expression "in" expression)
                let-exp)

    (expression ("unpack" (arbno identifier) "=" expression "in" expression)
                unpack-exp)

    (expression ("proc" "(" identifier ")" expression)
                proc-exp)

    (expression ("(" expression expression ")")
                call-exp)

    (expression ("%lexref" number)
                nameless-var-exp)

    (expression ("%let" expression "in" expression)
                nameless-let-exp)

    (expression ("%unpack" expression "in" expression)
                nameless-unpack-exp)

    (expression ("%lexproc" expression)
                nameless-proc-exp)))

(sllgen:make-define-datatypes scanner-spec grammar)

(define parse
  (sllgen:make-string-parser scanner-spec grammar))
