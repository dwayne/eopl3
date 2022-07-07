#lang eopl

;; Exercise 2.29
;;
;; Write a define-datatype and a parser for the following grammar:
;;
;; Lc-exp ::= Identifier
;;            [var-exp (var)]
;;
;;        ::= (lambda ({Identifier}*) Lc-exp)
;;            [lambda-exp (bound-vars body)]
;;
;;        ::= (Lc-exp {Lc-exp}*)
;;            [app-exp (rator rands)]

(provide

 ;; Build
 var-exp lambda-exp app-exp

 ;; Parse
 parse-expression)

(define-datatype lc-exp lc-exp?
  [var-exp
   (var identifier?)]
  [lambda-exp
   (bound-vars (list-of identifier?))
   (body lc-exp?)]
  [app-exp
   (rator lc-exp?)
   (rands (list-of lc-exp?))])

(define (parse-expression datum)
  (cond
    [(identifier? datum) (var-exp datum)]
    [(and (list? datum)
          (= (length datum) 3)
          (eq? (car datum) 'lambda)
          ((list-of identifier?) (cadr datum)))
     (lambda-exp (cadr datum) (parse-expression (caddr datum)))]
    [(and (list? datum)
          (>= (length datum) 1))
     (app-exp (parse-expression (car datum))
              (map parse-expression (cdr datum)))]
    [else
     (eopl:error 'parse-expression "Invalid concrete syntax: ~s" datum)]))

;; Helpers

(define (identifier? v)
  (and (symbol? v)
       (not (eq? v 'lambda))))

(define (list-of pred)
  (lambda (v)
    (or (null? v)
        (and
         (pair? v)
         (pred (car v))
         ((list-of pred) (cdr v))))))
