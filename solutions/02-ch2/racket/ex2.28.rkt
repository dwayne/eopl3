#lang eopl

;; Exercise 2.28
;;
;; Write an unparser that converts the abstract syntax of an Lc-exp into a
;; string that matches the following grammar:
;;
;; Lc-exp ::= Identifier
;;        ::= (lambda (Identifier) Lc-exp)
;;        ::= (Lc-exp Lc-exp)

(provide

 ;; Build
 var-exp lambda-exp app-exp

 ;; Unparse
 unparse-lc-exp)

(define-datatype lc-exp lc-exp?
  [var-exp
   (var identifier?)]
  [lambda-exp
   (bound-var symbol?)
   (body lc-exp?)]
  [app-exp
   (rator lc-exp?)
   (rand lc-exp?)])

(define (unparse-lc-exp exp)
  (cases lc-exp exp
    [var-exp (var)
             (symbol->string var)]
    [lambda-exp (bound-var body)
                (string-append "(lambda ("
                               (symbol->string bound-var)
                               ") "
                               (unparse-lc-exp body)
                               ")")]
    [app-exp (rator rand)
             (string-append "("
                            (unparse-lc-exp rator)
                            " "
                            (unparse-lc-exp rand)
                            ")")]))

;; Helpers

(define (identifier? v)
  (and (symbol? v)
       (not (eq? v 'lambda))))
