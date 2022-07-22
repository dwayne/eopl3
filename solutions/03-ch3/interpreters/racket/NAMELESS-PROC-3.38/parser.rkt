#lang eopl

;; Program    ::= Expression
;;
;; Expression ::= Number
;;
;;            ::= Identifier
;;
;;            ::= -(Expression, Expression)
;;
;;            ::= zero?(Expression)
;;
;;            ::= if Expression then Expression else Expression
;;
;;            ::= cond {Expression ==> Expression}* end
;;
;;            ::= let Identifier = Expression in Expression
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
 zero?-exp
 if-exp
 cond-exp
 let-exp
 proc-exp
 call-exp

 nameless-var-exp
 nameless-let-exp
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

    (expression ("zero?" "(" expression ")")
                zero?-exp)

    (expression ("if" expression "then" expression "else" expression)
                if-exp)

    (expression ("cond" (arbno expression "==>" expression) "end")
                cond-exp)

    (expression ("let" identifier "=" expression "in" expression)
                let-exp)

    (expression ("proc" "(" identifier ")" expression)
                proc-exp)

    (expression ("(" expression expression ")")
                call-exp)

    (expression ("%lexref" number)
                nameless-var-exp)

    (expression ("%let" expression "in" expression)
                nameless-let-exp)

    (expression ("%lexproc" expression)
                nameless-proc-exp)))

(sllgen:make-define-datatypes scanner-spec grammar)

(define parse
  (sllgen:make-string-parser scanner-spec grammar))