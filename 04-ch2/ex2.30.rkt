#lang eopl

(define-datatype lc-exp lc-exp?
  (var-exp (var identifier?))
  (lambda-exp (bound-vars identifier?) (body lc-exp?))
  (app-exp (rator lc-exp?) (rand lc-exp?)))

(define (identifier? v)
  (and (symbol? v) (not (eqv? v 'lambda))))

(define (parse-expression datum)
  (cond
    ((identifier? datum) (var-exp datum))
    ((pair? datum)
     (if (eqv? (car datum) 'lambda)
         ; lambda-exp
         (if (list? datum)
             (if (= (length datum) 3)
                 (if (list? (cadr datum))
                     (if (= (length (cadr datum)) 1)
                         (if (identifier? (car (cadr datum)))
                             (lambda-exp
                              (car (cadr datum))
                              (parse-expression (caddr datum)))
                             (eopl:error 'parse-expression "expected an identifier: ~s" (car (cadr datum))))
                         (eopl:error 'parse-expression "expected exactly one bound identifier: ~s" (cadr datum)))
                     (eopl:error 'parse-expression "expected a list containing exactly one bound identifier: ~s" (cadr datum)))
                 (eopl:error 'parse-expression "too few or too many terms in the lambda expression: ~s" datum))
             (eopl:error 'parse-expression "malformed lambda expression: ~s" datum))

         ; app-exp
         (if (list? datum)
             (if (= (length datum) 2)
                 (app-exp
                  (parse-expression (car datum))
                  (parse-expression (cadr datum)))
                 (eopl:error 'parse-expression "too few or too many terms in the application: ~s" datum))
             (eopl:error 'parse-expression "malformed application: ~s" datum))))
    (else
     (eopl:error 'parse-expression "unknown expression: ~s" datum))))

;; Uncomment any of the expressions below to see the error message it reports.

;(parse-expression 1)
; => unknown expression: 1

;(parse-expression '(lambda (2) x))
; => expected an identifier: 2

;(parse-expression '(lambda (x y) x))
; => expected exactly one bound identifier: (x y)

;(parse-expression '(lambda x x))
; => expected a list containing exactly one bound identifier: x

;(parse-expression '(lambda))
; => too few or too many terms in the lambda expression: (lambda)
;(parse-expression '(lambda (x) x y))
; => too few or too many terms in the lambda expression: (lambda (x) x y)

;(parse-expression '(lambda . 1))
; => malformed lambda expression: (lambda . 1)

;(parse-expression '(a))
; => too few or too many terms in the application: (a)
;(parse-expression '(a b c))
; => too few or too many terms in the application: (a b c)

;(parse-expression '(a . b))
; => malformed application: (a . b)