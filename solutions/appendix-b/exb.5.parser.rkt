#lang eopl

;; Exercise B.5
;;
;; Extend the language to include unary minus.
;;
;; Arith-expr        ::= Arith-term {Additive-op Arith-term}*
;; Arith-term        ::= Arith-unary {Multiplicative-op Arith-unary}*
;; Arith-unary       ::= Arith-factor | - Arith-factor
;; Arith-factor      ::= Number
;;                   ::= Variable
;;                   ::= ( Arith-expr )
;; Additive-op       ::= + | -
;; Multiplicative-op ::= * | /

(provide

 ;; AST
 arith-expr sum
 arith-term prod
 arith-unary pos neg
 arith-factor const var group
 additive-op add sub
 multiplicative-op mul div

 ;; Parser
 parse)

(define scanner-spec
  '((number (digit (arbno digit)) number)
    (variable (letter (arbno letter)) symbol)))

(define grammar
  '((arith-expr (arith-term (arbno additive-op arith-term)) sum)
    (arith-term (arith-unary (arbno multiplicative-op arith-unary)) prod)
    (arith-unary (arith-factor) pos)
    (arith-unary ("-" arith-factor) neg)
    (arith-factor (number) const)
    (arith-factor (variable) var)
    (arith-factor ("(" arith-expr ")") group)
    (additive-op ("+") add)
    (additive-op ("-") sub)
    (multiplicative-op ("*") mul)
    (multiplicative-op ("/") div)))

(sllgen:make-define-datatypes scanner-spec grammar)

(define parse
  (sllgen:make-string-parser scanner-spec grammar))
