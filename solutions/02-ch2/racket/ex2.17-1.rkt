#lang racket

(provide
 var-exp lambda-exp app-exp
 var-exp? lambda-exp? app-exp?
 var-exp->var
 lambda-exp->bound-var lambda-exp->body
 app-exp->rator app-exp->rand)

;; Constructors

(define (var-exp var) (list 'var-exp var))
(define (lambda-exp bound-var body) (list 'lambda-exp bound-var body))
(define (app-exp rator rand) (list 'app-exp rator rand))

;; Predicates

(define (var-exp? exp)
  (match exp
    [(list 'var-exp var) #t]
    [_ #f]))

(define (lambda-exp? exp)
  (match exp
    [(list 'lambda-exp bound-var body) #t]
    [_ #f]))

(define (app-exp? exp)
  (match exp
    [(list 'app-exp rator rand) #t]
    [_ #f]))

;; Extractors

(define (var-exp->var exp) (cadr exp))

(define (lambda-exp->bound-var exp) (cadr exp))
(define (lambda-exp->body exp) (caddr exp))

(define (app-exp->rator exp) (cadr exp))
(define (app-exp->rand exp) (caddr exp))
