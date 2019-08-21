#lang eopl

(define-datatype lc-exp lc-exp?
  (var-exp (var identifier?))
  (lambda-exp (bound-var identifier?) (body lc-exp?))
  (app-exp (rator lc-exp?) (rand lc-exp?)))

(define (identifier? v)
  (and (symbol? v) (not (eqv? v 'lambda))))

(define (unparse-lc-exp exp)
  (cases lc-exp exp
    (var-exp (var) (symbol->string var))
    (lambda-exp (bound-var body)
                (string-append
                 "(lambda ("
                 (symbol->string bound-var)
                 ") "
                 (unparse-lc-exp body)
                 ")"))
    (app-exp (rator rand)
             (string-append
              "("
              (unparse-lc-exp rator)
              " "
              (unparse-lc-exp rand)
              ")"))))

(define EXP1
  (app-exp (lambda-exp 'a (app-exp (var-exp 'a) (var-exp 'b)))
           (var-exp 'c)))

(define EXP2
  (lambda-exp 'x (lambda-exp 'y (app-exp (lambda-exp 'x (app-exp (var-exp 'x)
                                                                 (var-exp 'y)))
                                         (var-exp 'x)))))

(eopl:pretty-print (unparse-lc-exp EXP1))
(eopl:pretty-print (unparse-lc-exp EXP2))
