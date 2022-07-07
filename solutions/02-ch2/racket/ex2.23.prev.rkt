#lang eopl

(define-datatype lc-exp lc-exp?
  (var-exp (var identifier?))
  (lambda-exp (bound-var identifier?) (body lc-exp?))
  (app-exp (rator lc-exp?) (rand lc-exp?)))

(define (identifier? v)
  (and (symbol? v) (not (eqv? v 'lambda))))

; Uncomment to see the error produced since the identifier is named "lambda".
;
; (var-exp 'lambda)