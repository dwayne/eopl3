#lang eopl

(define-datatype env env?
  (empty-env)
  (extend-env (saved-var symbol?) (saved-val always?) (saved-env env?)))

(define (has-binding? current-env search-var)
  (cases env current-env
    (empty-env () #f)
    (extend-env (saved-var saved-val saved-env)
                (or (eqv? search-var saved-var)
                    (has-binding? saved-env search-var)))))

(eopl:pretty-print (has-binding? (extend-env 'a 1 (empty-env)) 'a)) ; #t
(eopl:pretty-print (has-binding? (extend-env 'a 1 (empty-env)) 'b)) ; #f