#lang eopl

(require "./parser.rkt")

(provide

 ;; Build
 empty-env
 extend-env
 extend-env-rec

 ;; Query
 env?
 apply-env

 identifier? identifier=?)

(define (empty-env)
  (lambda (search-var construct-proc-val)
    (eopl:error 'apply-env "No binding for ~s" search-var)))

(define (extend-env var val env)
  (lambda (search-var construct-proc-val)
    (if (identifier=? search-var var)
        val
        (apply-env env search-var construct-proc-val))))

(define (extend-env-rec proc-name var proc-body env)
  (letrec ([env-rec (lambda (search-var construct-proc-val)
                      (if (identifier=? search-var proc-name)
                          (construct-proc-val var proc-body env-rec)
                          (apply-env env search-var construct-proc-val)))])
    env-rec))

(define env? procedure?)

(define (apply-env env search-var construct-proc-val)
  (env search-var construct-proc-val))

(define identifier? symbol?)

(define identifier=? eq?)
