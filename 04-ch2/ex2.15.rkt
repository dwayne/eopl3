#lang eopl

; An implementation using the representation specified by the following grammar:
;
; Lc-exp ::= Identifier
;        ::= (lambda (Identifier) Lc-exp)
;        ::= (Lc-exp Lc-exp)

; The constructors

(define (var-exp var)
  var)

(define (lambda-exp bound-var body)
  (list 'lambda (list bound-var) body))

(define (app-exp rator rand)
  (list rator rand))

; The predicates

(define (var-exp? exp)
  (symbol? exp))

(define (lambda-exp? exp)
  (and (list? exp)
       (eqv? (car exp) 'lambda)))

(define (app-exp? exp)
  (and (list? exp)
       (= (length exp) 2)))

; The extractors

(define (var-exp->var exp)
  exp)

(define (lambda-exp->bound-var exp)
  (caadr exp))

(define (lambda-exp->body exp)
  (caddr exp))

(define (app-exp->rator exp)
  (car exp))

(define (app-exp->rand exp)
  (cadr exp))

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