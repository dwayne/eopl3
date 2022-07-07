#lang eopl

(provide

 ;; Build
 empty-env
 extend-env
 extend-env*

 ;; Query
 env?
 apply-env

 identifier? identifier=?)

(define-datatype env env?
  [empty]
  [extend
   (var identifier?)
   (val any?)
   (saved-env env?)])

(define (empty-env)
  (empty))

(define (extend-env var val env)
  (extend var val env))

(define (extend-env* vars vals env)
  (if (null? vars)
      env
      (extend-env* (cdr vars)
                   (cdr vals)
                   (extend-env (car vars)
                               (car vals)
                               env))))

(define (apply-env env1 search-var)
  (cases env env1
    [empty ()
           (eopl:error 'apply-env "No binding for ~s" search-var)]

    [extend (saved-var saved-val saved-env)
            (if (identifier=? search-var saved-var)
                saved-val
                (apply-env saved-env search-var))]))

(define identifier? symbol?)

(define identifier=? eq?)

(define (any? v) #t)
