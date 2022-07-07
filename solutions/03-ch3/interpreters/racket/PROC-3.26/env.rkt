#lang eopl

(require "./set.rkt")

(provide

 ;; Build
 empty-env
 extend-env

 ;; Query
 env?
 apply-env

 keep-free-vars

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

(define (apply-env env1 search-var)
  (cases env env1
    [empty ()
           (eopl:error 'apply-env "No binding for ~s" search-var)]

    [extend (saved-var saved-val saved-env)
            (if (identifier=? search-var saved-var)
                saved-val
                (apply-env saved-env search-var))]))

(define (keep-free-vars free-variables env1)
  (cases env env1
    [empty ()
           env1]

    [extend (saved-var saved-val saved-env)
            (if (member-of-set? saved-var free-variables)
                ;; Keep the free variables
                (extend saved-var
                        saved-val
                        (keep-free-vars
                         ;; But only the first occurrence
                         ;; So, if x and y are free variables and
                         ;; the environment is ((x 1) (y 2) (x 3))
                         ;; then the new environment will be ((x 1) (y 2))
                         ;; i.e. we don't need to keep the binding (x 3) since
                         ;; it is shadowed by the binding (x 1)
                         (diff-set free-variables (singleton-set saved-var))
                         saved-env))

                ;; Remove the bound variables
                (keep-free-vars free-variables saved-env))]))

(define identifier? symbol?)

(define identifier=? eq?)

(define (any? v) #t)
