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
;;            ::= let Identifier = Expression in Expression
;;
;;            ::= lazylet Identifier = Expression in Expression
;;
;;            ::= proc (Identifier) Expression
;;
;;            ::= letrec {Identifier (Identifier) = Expression}* in Expression
;;
;;            ::= (Expression Expression)
;;
;;            ::= begin Expression {; Expression}* end
;;
;;            ::= set Identifier = Expression

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
 let-exp
 lazylet-exp
 proc-exp
 letrec-exp
 call-exp
 begin-exp
 assign-exp

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

    (expression ("let" identifier "=" expression "in" expression)
                let-exp)

    (expression ("lazylet" identifier "=" expression "in" expression)
                lazylet-exp)

    (expression ("proc" "(" identifier ")" expression)
                proc-exp)

    (expression ("letrec" (arbno identifier "(" identifier ")" "=" expression) "in" expression)
                letrec-exp)

    (expression ("(" expression expression ")")
                call-exp)

    (expression ("begin" expression (arbno ";" expression) "end")
                begin-exp)

    (expression ("set" identifier "=" expression)
                assign-exp)))

(sllgen:make-define-datatypes scanner-spec grammar)

(define parse
  (sllgen:make-string-parser scanner-spec grammar))
