#lang eopl

(require "./parser.rkt")

(provide

 ;; Build
 empty-env
 extend-env
 extend-env-parallel
 extend-env-rec

 ;; Query
 env?
 apply-env

 identifier? identifier=?)

(define-datatype env env?
  [empty]
  [extend
   (var identifier?)
   (val any?)
   (saved-env env?)]
  [extend-parallel
   (vars list?)
   (vals list?)
   (saved-env env?)]
  [extend-rec
   (p-name identifier?)
   (b-var identifier?)
   (p-body expression?)
   (saved-env env?)])

(define (empty-env)
  (empty))

(define (extend-env var val env)
  (extend var val env))

(define (extend-env-parallel vars vals env)
  (extend-parallel vars vals env))

(define (extend-env-rec proc-name var proc-body env)
  (extend-rec proc-name var proc-body env))

(define (apply-env env1 search-var construct-proc-val)
  (cases env env1
    [empty ()
           (eopl:error 'apply-env "No binding for ~s" search-var)]

    [extend (saved-var saved-val saved-env)
            (if (identifier=? search-var saved-var)
                saved-val
                (apply-env saved-env search-var construct-proc-val))]

    [extend-parallel (vars vals saved-env)
                     (let ([saved-val (get-val search-var vars vals)])
                       (if saved-val
                           saved-val
                           (apply-env saved-env search-var construct-proc-val)))]

    [extend-rec (p-name b-var p-body saved-env)
                (if (identifier=? search-var p-name)
                    (construct-proc-val b-var p-body env1)
                    (apply-env saved-env search-var construct-proc-val))]))

(define (get-val search-var vars vals)
  (if (null? vars)
      #f
      (if (identifier=? search-var (car vars))
          (car vals)
          (get-val search-var (cdr vars) (cdr vals)))))

(define identifier? symbol?)

(define identifier=? eq?)

(define (any? v) #t)
