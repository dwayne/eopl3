#lang eopl

(define-datatype lc-exp lc-exp?
  (var-exp (var identifier?))
  (lambda-exp (bound-vars (list-of identifier?)) (body lc-exp?))
  (app-exp (rator lc-exp?) (rand (list-of lc-exp?))))

(define (identifier? v)
  (and (symbol? v) (not (eqv? v 'lambda))))

(define (parse-expression datum)
  (cond
    ((symbol? datum) (var-exp datum))
    ((pair? datum)
     (if (eqv? (car datum) 'lambda)
         (lambda-exp
          (cadr datum)
          (parse-expression (caddr datum)))
         (app-exp
          (parse-expression (car datum))
          (map parse-expression (cdr datum)))))
    (else
     (eopl:error 'parse-expression "invalid syntax"))))

(eopl:pretty-print (parse-expression '(lambda () x)))
(eopl:pretty-print (parse-expression '(lambda (x) x)))
(eopl:pretty-print (parse-expression '((lambda (x y) (+ x y)) a b)))