#lang eopl

;; Program    ::= Statement
;;
;; Statement  ::= Identifier = Expression
;;
;;            ::= print Expression
;;
;;            ::= { {Statement}*(;) }
;;
;;            ::= if Expression Statement Statement
;;
;;            ::= while Expression Statement
;;
;;            ::= var {Identifier}*(,) ; Statement
;;
;; Expression ::= Number
;;
;;            ::= Identifier
;;
;;            ::= -(Expression, Expression)
;;
;;            ::= +(Expression, Expression)
;;
;;            ::= *(Expression, Expression)
;;
;;            ::= zero?(Expression)
;;
;;            ::= not(Expression)
;;
;;            ::= if Expression then Expression else Expression
;;
;;            ::= let Identifier = Expression in Expression
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

 statement
 assign-stmt
 print-stmt
 block-stmt
 if-stmt
 while-stmt
 var-stmt

 expression expression?
 const-exp
 var-exp
 diff-exp
 add-exp
 mul-exp
 zero?-exp
 not-exp
 if-exp
 let-exp
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
  '((program (statement)
             a-program)

    (statement (identifier "=" expression)
               assign-stmt)

    (statement ("print" expression)
               print-stmt)

    (statement ("{" (separated-list statement ";") "}")
               block-stmt)

    (statement ("if" expression statement statement)
               if-stmt)

    (statement ("while" expression statement)
               while-stmt)

    (statement ("var" (separated-list identifier ",") ";" statement)
               var-stmt)

    (expression (number)
                const-exp)

    (expression (identifier)
                var-exp)

    (expression ("-" "(" expression "," expression ")")
                diff-exp)

    (expression ("+" "(" expression "," expression ")")
                add-exp)

    (expression ("*" "(" expression "," expression ")")
                mul-exp)

    (expression ("zero?" "(" expression ")")
                zero?-exp)

    (expression ("not" "(" expression ")")
                not-exp)

    (expression ("if" expression "then" expression "else" expression)
                if-exp)

    (expression ("let" identifier "=" expression "in" expression)
                let-exp)

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
