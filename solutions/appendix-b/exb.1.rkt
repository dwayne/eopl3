#lang eopl

;; Exercise B.1
;;
;; Write a lexical specification and a grammar in SLLGEN that will scan and parse
;; strings according to the following grammar:
;;
;; Arith-expr        ::= Arith-term {Additive-op Arith-term}*
;; Arith-term        ::= Arith-factor {Multiplicative-op Arith-factor}*
;; Arith-factor      ::= Number
;;                   ::= ( Arith-expr )
;; Additive-op       ::= + | -
;; Multiplicative-op ::= * | /

(provide

 ;; AST
 arith-expr sum
 arith-term prod
 arith-factor const group
 additive-op add sub
 multiplicative-op mul div

 ;; Parser
 parse)

(define scanner-spec
  '((number (digit (arbno digit)) number)))

(define grammar
  '((arith-expr (arith-term (arbno additive-op arith-term)) sum)
    (arith-term (arith-factor (arbno multiplicative-op arith-factor)) prod)
    (arith-factor (number) const)
    (arith-factor ("(" arith-expr ")") group)
    (additive-op ("+") add)
    (additive-op ("-") sub)
    (multiplicative-op ("*") mul)
    (multiplicative-op ("/") div)))

(sllgen:make-define-datatypes scanner-spec grammar)

(define parse
  (sllgen:make-string-parser scanner-spec grammar))
