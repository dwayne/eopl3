#lang eopl

; An implementation using the representation specified by the following:
;
; Lc-exp ::= (var Identifier)
;        ::= (lambda Identifier Lc-exp)
;        ::= (app Lc-exp Lc-exp)

; The constructors

(define (var-exp var)
  (list 'var var))

(define (lambda-exp bound-var body)
  (list 'lambda bound-var body))

(define (app-exp rator rand)
  (list 'app rator rand))

; The predicates

(define (var-exp? exp)
  (eqv? (car exp) 'var))

(define (lambda-exp? exp)
  (eqv? (car exp) 'lambda))

(define (app-exp? exp)
  (eqv? (car exp) 'app))

; The extractors

(define (var-exp->var exp)
  (cadr exp))

(define (lambda-exp->bound-var exp)
  (cadr exp))

(define (lambda-exp->body exp)
  (caddr exp))

(define (app-exp->rator exp)
  (cadr exp))

(define (app-exp->rand exp)
  (caddr exp))

; occurs-free? : Sym x Lc-exp -> Bool
(define (occurs-free? search-var exp)
  (cond
    [(var-exp? exp)
     (eqv? search-var (var-exp->var exp))]
    [(lambda-exp? exp)
     (and
      (not (eqv? search-var (lambda-exp->bound-var exp)))
      (occurs-free? search-var (lambda-exp->body exp)))]
    [else
     (or
      (occurs-free? search-var (app-exp->rator exp))
      (occurs-free? search-var (app-exp->rand exp)))]))

(eopl:pretty-print (occurs-free? 'x (var-exp 'x))) ; #t
(eopl:pretty-print (occurs-free? 'x (var-exp 'y))) ; #f

; (lambda (y) (y x))
(eopl:pretty-print (occurs-free? 'x (lambda-exp
                                     'y
                                     (app-exp (var-exp 'y) (var-exp 'x))))) ; #t
(eopl:pretty-print (occurs-free? 'y (lambda-exp
                                     'y
                                     (app-exp (var-exp 'y) (var-exp 'x))))) ; #f