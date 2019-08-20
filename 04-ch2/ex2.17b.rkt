#lang eopl

; An implementation using a procedural representation.
;
; The grammar:
;
; Lc-exp ::= Identifier
;        ::= (lambda (Identifier) Lc-exp)
;        ::= (Lc-exp Lc-exp)

; The constructors

(define (var-exp var)
  (list (lambda (extractor)
          (cond
            [(eq? extractor 'var-exp->var) var]))
        (lambda (predicate)
          (eq? predicate 'var-exp?))))

(define (lambda-exp bound-var body)
  (list (lambda (extractor)
          (cond
            [(eq? extractor 'lambda-exp->bound-var) bound-var]
            [(eq? extractor 'lambda-exp->body) body]))
        (lambda (predicate)
          (eq? predicate 'lambda-exp?))))

(define (app-exp rator rand)
  (list (lambda (extractor)
          (cond
            [(eq? extractor 'app-exp->rator) rator]
            [(eq? extractor 'app-exp->rand) rand]))
        (lambda (predicate)
          (eq? predicate 'app-exp?))))

; The predicates

(define (var-exp? exp)
  ((cadr exp) 'var-exp?))

(define (lambda-exp? exp)
  ((cadr exp) 'lambda-exp?))

(define (app-exp? exp)
  ((cadr exp) 'app-exp?))

; The extractors

(define (var-exp->var exp)
  ((car exp) 'var-exp->var))

(define (lambda-exp->bound-var exp)
  ((car exp) 'lambda-exp->bound-var))

(define (lambda-exp->body exp)
  ((car exp) 'lambda-exp->body))

(define (app-exp->rator exp)
  ((car exp) 'app-exp->rator))

(define (app-exp->rand exp)
  ((car exp) 'app-exp->rand))

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