#lang eopl

;; Exercise B.4
;;
;; Extend the language to include variables.
;;
;; Arith-expr        ::= Arith-term {Additive-op Arith-term}*
;; Arith-term        ::= Arith-factor {Multiplicative-op Arith-factor}*
;; Arith-factor      ::= Number
;;                   ::= Variable
;;                   ::= ( Arith-expr )
;; Additive-op       ::= + | -
;; Multiplicative-op ::= * | /

(provide

 ;; AST
 arith-expr sum
 arith-term prod
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
    (arith-term (arith-factor (arbno multiplicative-op arith-factor)) prod)
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
