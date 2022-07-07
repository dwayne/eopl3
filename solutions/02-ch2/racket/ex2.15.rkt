#lang racket

(provide
 var-exp lambda-exp app-exp
 var-exp? lambda-exp? app-exp?
 var-exp->var
 lambda-exp->bound-var lambda-exp->body
 app-exp->rator app-exp->rand)

;; Constructors

(define (var-exp var) var)

(define (lambda-exp bound-var body)
  `(lambda (,bound-var) ,body))

(define (app-exp rator rand)
  `(,rator ,rand))

;; Predicates

(define (var-exp? exp) (symbol? exp))

(define (lambda-exp? exp)
  (match exp
    [(list 'lambda (list bound-var) body) #t]
    [_ #f]))

(define (app-exp? exp)
  (match exp
    [(list rator rand) #t]
    [_ #f]))

;; Extractors

(define (var-exp->var exp) exp)

(define (lambda-exp->bound-var exp) (caadr exp))
(define (lambda-exp->body exp) (caddr exp))

(define (app-exp->rator exp) (car exp))
(define (app-exp->rand exp) (cadr exp))
