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
;;            ::= proc (Identifier) Expression
;;
;;            ::= letrec {Identifier (Identifier) = Expression}* in Expression
;;
;;            ::= (Expression Expression)
;;
;;            ::= begin Expression {; Expression}* end
;;
;;            ::= set Identifier = Expression
;;
;;            ::= pair(Expression, Expression)
;;
;;            ::= left(Expression)
;;
;;            ::= right(Expression)
;;
;;            ::= setleft(Expression, Expression)
;;
;;            ::= setright(Expression, Expression)
;;
;;            ::= newarray(Expression, Expression)
;;
;;            ::= arrayref(Expression, Expression)
;;
;;            ::= arrayset(Expression, Expression, Expression)
;;
;;            ::= arraylength(Expression)

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
 proc-exp
 letrec-exp
 call-exp
 begin-exp
 assign-exp
 newpair-exp
 left-exp
 right-exp
 setleft-exp
 setright-exp
 newarray-exp
 arrayref-exp
 arrayset-exp
 arraylength-exp

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

    (expression ("proc" "(" identifier ")" expression)
                proc-exp)

    (expression ("letrec" (arbno identifier "(" identifier ")" "=" expression) "in" expression)
                letrec-exp)

    (expression ("(" expression expression ")")
                call-exp)

    (expression ("begin" expression (arbno ";" expression) "end")
                begin-exp)

    (expression ("set" identifier "=" expression)
                assign-exp)

    (expression ("pair" "(" expression "," expression ")")
                newpair-exp)

    (expression ("left" "(" expression ")")
                left-exp)

    (expression ("right" "(" expression ")")
                right-exp)

    (expression ("setleft" "(" expression "," expression ")")
                setleft-exp)

    (expression ("setright" "(" expression "," expression ")")
                setright-exp)

    (expression ("newarray" "(" expression "," expression ")")
                newarray-exp)

    (expression ("arrayref" "(" expression "," expression ")")
                arrayref-exp)

    (expression ("arrayset" "(" expression "," expression "," expression ")")
                arrayset-exp)

    (expression ("arraylength" "(" expression ")")
                arraylength-exp)))

(sllgen:make-define-datatypes scanner-spec grammar)

(define parse
  (sllgen:make-string-parser scanner-spec grammar))
