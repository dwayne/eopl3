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

(define-datatype env env?
  [empty]
  [extend
   (var identifier?)
   (val any?)
   (saved-env env?)]
  [extend-rec
   (p-names (list-of identifier?))
   (b-vars (list-of identifier?))
   (p-bodys (list-of expression?))
   (saved-env env?)])

(define (empty-env)
  (empty))

(define (extend-env var val env)
  (extend var val env))

(define (extend-env-rec proc-names vars proc-bodys env)
  (extend-rec proc-names vars proc-bodys env))

(define (apply-env env1 search-var construct-proc-val)
  (cases env env1
    [empty ()
           (eopl:error 'apply-env "No binding for ~s" search-var)]

    [extend (saved-var saved-val saved-env)
            (if (identifier=? search-var saved-var)
                saved-val
                (apply-env saved-env search-var construct-proc-val))]

    [extend-rec (p-names b-vars p-bodys saved-env)
                (let ([args (find-args p-names b-vars p-bodys search-var)])
                  (if args
                      (construct-proc-val (car args) (cadr args) env1)
                      (apply-env saved-env search-var construct-proc-val)))]))

(define (find-args p-names b-vars p-bodys search-var)
  (if (null? p-names)
      #f
      (if (identifier=? search-var (car p-names))
          (list (car b-vars) (car p-bodys))
          (find-args (cdr p-names)
                     (cdr b-vars)
                     (cdr p-bodys)
                     search-var))))

(define identifier? symbol?)

(define identifier=? eq?)

(define (any? v) #t)
